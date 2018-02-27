;; instantiate EFFECT and OP instance as well as some lookup table for the facts.
;; EFFECT and OP are primitive representation of operators and its effects.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *fact-index*)
(defvar *fact-size*)
(defvar *fact-trie*)
(defvar *state-size*)
(defvar *op-index*)
(defvar *instantiated-ops*)
(defvar *op-size*)
(defvar *instantiated-axiom-layers*)
(defvar *instantiated-init*)
(defvar *instantiated-goal*)

;; conditions and effects are represented by a fixnum index to a fact.
;; however, the fixnum can be negative, in which case it represent a negative condition or a delete effect.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((con () (make-a-array 16 :element-type 'fixnum :initial-element most-positive-fixnum)))
    (defstruct effect
      (con (con) :type (array fixnum))
      (eff most-positive-fixnum :type fixnum))
    
    (defstruct op
      (pre (con) :type (array fixnum))
      (eff (make-a-array 16
                         :element-type 'effect
                         :initial-element +uninitialized-effect+)
           :type (array effect)))))

(defvar +uninitialized-effect+ (make-effect))

(deftype axiom-layer ()
  '(array effect))

(declaim (strips.lib:index *fact-index* *op-index*))
(declaim (fixnum *fact-size* *state-size* *op-size*))
(declaim (cons *fact-trie*))
(declaim ((array op) *instantiated-ops*))
(declaim ((array axiom-layer) *instantiated-axiom-layers*))
(declaim ((array fixnum) *instantiated-init*))
(declaim (fixnum *instantiated-goal*))

(defun instantiate (info)
  (with-parsed-information4 info
    (multiple-value-bind (fact-index fact-size fact-trie) (index-facts)
      (multiple-value-bind (op-index instantiated-ops) (instantiate-ops fact-index fact-trie)
        (list* :fact-index fact-index
               :fact-size fact-size
               :fact-trie fact-trie
               :state-size (strips.lib:index-size fact-index)
               :op-index op-index
               :instantiated-ops instantiated-ops
               :successor-generator (generate-sg instantiated-ops)
               :instantiated-axiom-layers (instantiate-axiom-layers fact-index fact-trie)
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
  (log:info "Instantiating operator objects")
  (let ((op-sexp-index (strips.lib:make-index :test 'equal))
        (op-index      (strips.lib:make-index :test 'equalp)))
    (log:info "Making an operator index")
    (dolist (o *ops*)
      (multiple-value-bind (id inserted) (strips.lib:index-insert op-index (instantiate-op o index trie))
        (declare (ignore id))
        (when inserted
          (strips.lib:index-insert op-sexp-index o))))

    (log:info "Removed duplicated operators: ~a -> ~a" (length *ops*) (strips.lib:index-size op-index))

    (values op-sexp-index
            (make-array (strips.lib:index-size op-index)
                        :element-type 'op
                        :initial-contents (strips.lib:index-array op-index)))))

(defun opposite-effect-p (a b)
  (match* (a b)
    (((effect :con con1 :eff eff1)
      (effect :con (equalp con1) :eff (= (lognot eff1))))
     t)))

(declaim (inline logabs))
(defun logabs (number)
  (if (minusp number)
      (lognot number)
      number))

(defun instantiate-op (op index trie)
  (ematch op
    (`((,name ,@args) ,reachable-effects)
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
             ((op pre :eff (place eff))
              (dolist (c (remove-duplicates gpre :test 'equal))
                (if (positive c)
                    (unless (static-p c)
                      (linear-extend pre (strips.lib:index-id index c) most-positive-fixnum))
                    (let ((i (strips.lib:index-id index (second c))))
                      (when i ; otherwise unreachable
                        (linear-extend pre (lognot i) most-positive-fixnum)))))
              (sort pre #'<)
              (iter (for e in geff)
                    (for i from 0)
                    (unless (member i reachable-effects)
                      (log:trace "op ~a:~%unreachable effect condition: ~a" `(,name ,@args) e))
                    (instantiate-effect e eff index trie))
              (setf eff (sort eff #'< :key #'effect-eff))
              (setf eff (delete-duplicates eff :test 'equalp))
              ;; postprocessing: when the effect-conditions are equivalent for the
              ;; positive and negative effect of the same literal, the effect should
              ;; be removed.
              (iter (with blacklist = nil)
                    (for e1 in-vector eff with-index i)
                    (generate j from 0)
                    (when (member i blacklist)
                      (next-iteration))
                    (iter (for e2 in-vector eff with-index k from (1+ i))
                          (with noop-found = nil)
                          (when (opposite-effect-p e1 e2)
                            (pushnew i blacklist)
                            (pushnew k blacklist)
                            (log:trace "op ~a:~%cancelling effects: ~a" `(,name ,@args)
                                       (strips.lib:index-ref index (logabs (effect-eff e1))))
                            (setf noop-found t))
                          (finally
                           (unless noop-found
                             (next j)
                             (setf (aref eff j) e1))))
                    (finally
                     (setf (fill-pointer eff) (1+ j))))))
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
       (iter (for c in (remove-duplicates ground-conditions :test 'equal))
             (if (positive c)
                 (unless (static-p c)
                   (linear-extend con (strips.lib:index-id index c) most-positive-fixnum))
                 (let ((i (strips.lib:index-id index (second c))))
                   (when i ; otherwise unreachable
                     (linear-extend con (lognot i) most-positive-fixnum)))))
       (sort con #'<)
       (when (positive atom)
         (strips.lib:query-trie
          (lambda (c) (setf eff (strips.lib:index-id index c)))
          trie atom))
       (when (negative atom)
         (strips.lib:query-trie
          (lambda (c)
            (let ((i (strips.lib:index-id index c))) ;; note: don't have to call SECOND
              (when i ; otherwise unreachable      ;       |  because it is done already
                (setf eff (lognot i)))))           ;       |
          trie (second atom)))                     ; <--- here
       ;; note: ignoring action cost at the moment
       ))
    (linear-extend effects e)))

(defun instantiate-axiom-layers (index trie)
  (let ((all-results (make-a-array (length *axiom-layers*)
                                   :element-type 'axiom-layer
                                   :initial-element (make-a-array 0
                                                                  :element-type 'effect
                                                                  :initial-element +uninitialized-effect+))))
    (iter (for layer in-sequence *axiom-layers*)
          (when layer ; skip the empty layers
            (let ((results (make-a-array (length layer)
                                         :element-type 'effect
                                         :initial-element +uninitialized-effect+)))
              (dolist (axiom layer)
                (instantiate-axiom axiom index trie results))
              (linear-extend all-results results))))
    all-results))

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
            (linear-extend results (strips.lib:index-id fact-index p))))
    results))

(defun instantiate-goal (fact-index)
  (strips.lib:index-id fact-index *goal*))


(defun find-lexical-variables (env)
  (mapcar #'car
          (sb-c::lexenv-vars
           (sb-c::coerce-to-lexenv env))))

(defmacro fcase9 (i &body body &environment env)
  "Jump-table based CASE implementation by myself
See https://gist.github.com/guicho271828/707be5ad51edb858ff751d954e37c267 for summary"
  (let* ((vars (find-lexical-variables env))
         (types (mapcar (rcurry #'introspect-environment:variable-type env) vars)))
    `(funcall
      (the (function ,types *)
           (svref (the (simple-vector ,(length body))
                       (load-time-value
                        (vector
                         ,@(iter (for b in body)
                                 (for j from 0)
                                 (collecting
                                  `(lambda (,@vars) (locally (declare ((eql ,j) ,i)) ,b)))))
                        t))
                  ,i))
      ,@vars)))

;; Example
#+(or)
(defun 256way/fcase9 (i)
  (let ((rand (random 10)))
    (fcase9 i
      . #.(loop :for x :from 0 :repeat 256
             :collect `(progn (* i rand))))))

(defparameter *effect-compilation-threashold* 3000)
(defmacro compiled-apply-op (op-id state child ops)
  (assert (symbolp ops))
  (if (< (length *instantiated-ops*) *effect-compilation-threashold*)
      (%compiled-apply-op op-id state child ops)
      (%interpret-apply-op op-id state child ops)))

(defun %compiled-apply-op (op-id state child ops)
  `(progn
     (fcase9 ,op-id
       ,@(iter (for op in-vector (symbol-value ops))
               (collecting
                `(progn
                   ,@(compile-apply-op op state child)))))
     ,child))

(defun compile-apply-op (op state child)
  (ematch op
    ((op eff)
     (iter (for e in-vector eff)
           (collecting
            (compile-apply-effect e state child))))))

(defun compile-apply-effect (effect state child)
  (ematch effect
    ((effect con eff)
     (let ((effect-form
            (if (minusp eff)
                `(setf (aref ,child ,(lognot eff)) 0)
                `(setf (aref ,child ,eff) 1)))
           (condition-form
            `(and ,@(iter (for i in-vector con)
                          (if (minusp i)
                              `(= 0 (aref ,state ,(lognot i)))
                              `(= 1 (aref ,state i)))))))
       (if (zerop (length con))
           effect-form
           `(when ,condition-form
              ,effect-form))))))

(defun %interpret-apply-op (op-id state child ops)
  (log:warn "falling back to the interpretation based apply-op")
  `(progn
     (ematch (aref (load-time-value ,ops t) ,op-id)
       ((op eff)
        (iter (for e in-vector eff)
              (apply-effect e ,state ,child))))
     ,child))
