;; instantiate EFFECT and OP instance as well as some lookup table for the facts.
;; EFFECT and OP are primitive representation of operators and its effects.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *fact-index*)
(defvar *fact-size*)
(defvar *fact-trie*)
(defvar *op-index*)
(defvar *instantiated-ops*)

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
               info)))))

(defun index-facts ()
  (let ((i (strips.lib:make-index :test 'equal))
        (trie (strips.lib:make-trie))
        (fact-size 0))
    ;; indexing init
    (dolist (f *init*)
      (unless (static-p f)
        (strips.lib:index-insert i f)
        (strips.lib:trie-insert trie f)))
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
  (flet ((con () (make-array 16
                             :element-type 'fixnum
                             :adjustable t
                             :fill-pointer 0
                             :initial-element most-positive-fixnum)))
    (defstruct effect
      (con (con) :type (array fixnum))
      (eff 0 :type fixnum))
    
    (defstruct op
      (pre (con) :type (array fixnum))
      (eff (make-array 16 :element-type 'effect :adjustable t :fill-pointer 0) :type (array effect)))))

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
                    (for i from 0)
                    (instantiate-effect e eff index trie))))
           op))))))

(defun instantiate-effect (e effects index trie)
  (match e
    (`(forall ,_ (when (and ,@conditions) ,atom))
      (instantiate-effect-aux conditions nil atom effects index trie))))

(defun instantiate-effect-aux (conditions ground-conditions atom effects index trie)
  "recurse into each possibility of universally quantified condition"
  (if conditions
      (strips.lib:query-trie
       (lambda (c)
         (let ((rest-conditions (copy-tree (rest conditions))))
           (iter (for a in (rest c))
                 (for p in (rest (first conditions)))
                 (when (variablep p)
                   (setf rest-conditions (nsubst a p rest-conditions))))
           (instantiate-effect-aux rest-conditions (cons c ground-conditions) atom effects index trie)))
       trie (first conditions))
      (instantiate-effect-aux2 ground-conditions atom effects index trie)))

(defun instantiate-effect-aux2 (ground-conditions atom effects index trie)
  (let ((e (make-effect)))
    (match e
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
