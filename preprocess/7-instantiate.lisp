;; instantiate EFFECT and OP instance as well as some lookup table for the facts.
;; EFFECT and OP are primitive representation of operators and its effects.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *fact-index*)
(defvar *fact-size*)
(defvar *fact-trie*)
(defvar *op-index*)
(defvar *instantiated-ops*)
(defvar *instantiated-axioms*)
(defvar *instantiated-init*)
(defvar *instantiated-goal*)

(defun instantiate (info)
  (with-parsed-information4 info
    (multiple-value-bind (fact-index fact-size fact-trie) (index-facts)
      (multiple-value-bind (op-index instantiated-ops) (instantiate-ops fact-index fact-trie)
        (list* :fact-index fact-index
               :fact-size fact-size
               :fact-trie fact-trie
               :op-index op-index
               :instantiated-ops instantiated-ops
               :successor-generator (generate-sg instantiated-ops)
               :instantiated-axioms (instantiate-axioms fact-index fact-trie)
               :instantiated-init (instantiate-init fact-index fact-size)
               :instantiated-goal (instantiate-goal fact-index)
               info)))))

(defun index-facts ()
  (let ((i (strips.lib:make-index :test 'equal))
        (trie (strips.lib:make-trie))
        (fact-size 0))
    ;; indexing init
    (dolist (f *init*)
      ;; index contains only fluent facts; however, trie contains all facts,
      ;; including static facts, because it is used for looking up the
      ;; candidates for free variables.  static facts are never added to the
      ;; preconditions nor effect conditions.
      (unless (static-p f)
        (strips.lib:index-insert i f))
      (strips.lib:trie-insert trie f))
    ;; indexing fluents
    (dolist (f *facts*)
      (strips.lib:index-insert i f)
      (strips.lib:trie-insert trie f))
    (setf fact-size (strips.lib:index-size i))
    ;; indexing axioms
    (dolist (f *ground-axioms*)
      (strips.lib:index-insert i f)
      (strips.lib:trie-insert trie f))
    (values i fact-size trie)))

(defun instantiate-ops (index trie)
  (values (let ((i (strips.lib:make-index :test 'equal)))
            (dolist (o *ops* i)
              (strips.lib:index-insert i o)))
          (map 'vector (lambda (op) (instantiate-op op index trie)) *ops*)))

;; conditions and effects are represented by a fixnum index to a fact.
;; however, the fixnum can be negative, in which case it represent a negative condition or a delete effect.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((con () (make-a-array 16 :element-type 'fixnum :initial-element most-positive-fixnum)))
    (defstruct effect
      (con (con) :type (array fixnum))
      (eff 0 :type fixnum))
    
    (defstruct op
      (pre (con) :type (array fixnum))
      (eff (make-a-array 16
                         :element-type 'effect
                         :initial-element +uninitialized-effect+)
           :type (array effect)))))

(define-constant +uninitialized-effect+ (make-effect) :test 'equalp)

(defun instantiate-op (op index trie)
  (ematch op
    (`((,name ,@args) ,_)
      (ematch (find name *actions* :key #'second)
        ((plist :parameters params
                :precondition `(and ,@precond)
                :effect `(and ,@effects))
         (let* ((gpre (copy-tree precond))
                (geff (copy-tree effects))
                (op (make-op)))
           (iter (for a in args)
                 (for p in params)
                 (setf gpre (nsubst a p gpre))
                 (setf geff (nsubst a p geff)))

           (match op
             ((op pre eff)
              (dolist (c gpre)
                (if (positive c)
                    (unless (static-p c)
                      (linear-extend pre (strips.lib:index index c)))
                    (let ((i (strips.lib:index index (second c))))
                      (when i ; otherwise unreachable
                        (linear-extend pre (lognot i))))))
              (sort pre #'<) 
              (iter (for e in geff)
                    (instantiate-effect e eff index trie))))
           op))))))

(defun instantiate-effect (e effects index trie)
  (match e
    (`(forall ,_ (when (and ,@conditions) ,atom))
      (instantiate-effect-aux conditions nil atom effects index trie))))

(defun instantiate-effect-aux (conditions ground-conditions atom effects index trie)
  "recurse into each possibility of universally quantified condition"
  (if conditions
      (ematch conditions
        ((list* first rest)
         (let* ((negative (negative first))
                (c (if negative (second first) first)))
           (strips.lib:query-trie
            (lambda (gc)
              (let ((rest (copy-tree rest)))
                (iter (for a in (rest gc))
                      (for p in (rest c))
                      (when (variablep p)
                        (setf rest (nsubst a p rest))))
                (instantiate-effect-aux rest (cons (if negative `(not ,gc) gc) ground-conditions)
                                        atom effects index trie)))
            trie c))))
      (instantiate-effect-aux2 ground-conditions atom effects index trie)))

(defun instantiate-effect-aux2 (ground-conditions atom effects index trie)
  (let ((e (make-effect)))
    (ematch e
      ((effect con :eff (place eff))
       (iter (for c in ground-conditions)
             (if (positive c)
                 (unless (static-p c)
                   (linear-extend con (strips.lib:index index c)))
                 (let ((i (strips.lib:index index (second c))))
                   (when i ; otherwise unreachable
                     (linear-extend con (lognot i))))))
       (sort con #'<)
       (when (positive atom)
         (strips.lib:query-trie
          (lambda (c) (setf eff (strips.lib:index index c)))
          trie atom))
       (when (negative atom)
         (strips.lib:query-trie
          (lambda (c)
            (let ((i (strips.lib:index index c))) ;; note: don't have to call SECOND
              (when i ; otherwise unreachable      ;       |  because it is done already
                (setf eff (lognot i)))))           ;       |
          trie (second atom)))                     ; <--- here
       ;; note: ignoring action cost at the moment
       ))
    (linear-extend effects e)))

(defun instantiate-axioms (index trie &aux (first-iteration t))
  (map 'vector
       (lambda (layer)
         (let ((results (make-a-array 32
                                      :element-type 'effect
                                      :initial-element +uninitialized-effect+)))
           (if first-iteration
               (setf first-iteration nil)
               (dolist (axiom layer)
                 (instantiate-axiom axiom index trie results)))
           results))
       *axiom-layers*))

(defun instantiate-axiom (axiom index trie results)
  (ematch axiom
    ((list* name args)
     (iter (for lifted in (remove-if-not (lambda-match ((list :derived `(,(eq name) ,@_) _) t)) *axioms*))
           (ematch lifted
             ((list :derived `(,(eq name) ,@params) `(and ,@body))
              (let ((gbody (copy-tree body)))
                (iter (for a in args)
                      (for p in params)
                      (setf gbody (nsubst a p gbody)))
                ;; need to instantiate each free variable
                (instantiate-effect-aux gbody nil axiom results index trie))))))))

(defun instantiate-init (fact-index fact-size)
  (let ((results (make-a-array fact-size :element-type 'fixnum)))
    (iter (for p in *init*)
          (unless (static-p p)
            (linear-extend results (strips.lib:index fact-index p))))
    results))

(defun instantiate-goal (fact-index)
  (strips.lib:index fact-index *goal*))
