;; instantiate EFFECT and OP instance as well as some lookup table for the facts.
;; EFFECT and OP are primitive representation of operators and its effects.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *index*)
(defvar *fluent-size*)
(defvar *trie*)
(defvar *instantiated-ops*)

(defun instantiate (info)
  (with-parsed-information4 info
    (multiple-value-bind (index fluent-size trie) (index-facts)
      (multiple-value-bind (op-index instantiated-ops) (instantiate-ops index trie)
        (list* :index index
               :fluent-size fluent-size
               :trie trie
               :op-index op-index
               :instantiated-ops instantiate-ops
               info)))))

(defun index-facts ()
  (let ((i (strips.lib:make-index :test 'equal))
        (trie (strips.lib:make-trie))
        (fluent-size 0))
    ;; indexing init
    (dolist (f *init*)
      (unless (static-p f)
        (strips.lib:index-insert i f)
        (strips.lib:trie-insert trie f)))
    ;; indexing fluents
    (dolist (f *facts*)
      (strips.lib:index-insert i f)
      (strips.lib:trie-insert trie f))
    (setf fluent-size (strips.lib:index-size i))
    ;; indexing axioms
    (dolist (f *ground-axioms*)
      (strips.lib:index-insert i f)
      (strips.lib:trie-insert trie f))
    (values i fluent-size trie)))

(defun instantiate-ops (index trie)
  (values (let ((i (strips.lib:make-index :test 'equal)))
            (dolist (o *ops* i)
              (strips.lib:index-insert i o)))
          (mapcar (lambda (op) (instantiate-op op index trie)) *ops*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((con () (make-array 16
                             :element-type 'fixnum
                             :adjustable t
                             :fill-pointer 0)))
    (defstruct effect
      (con+ (con) :type (array fixnum))
      (con- (con) :type (array fixnum))
      (add (con) :type (array fixnum))
      (del (con) :type (array fixnum)))
    
    (defstruct op
      (pre+ (con) :type (array fixnum))
      (pre- (con) :type (array fixnum))
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
             ((op pre+ pre- eff)
              (dolist (c gpre)
                (if (positive c)
                    (unless (static-p c)
                      (linear-extend pre+ (strips.lib:index index c)))
                    (let ((i (strips.lib:index index (second c))))
                      (when i ; otherwise unreachable
                        (linear-extend pre- i)))))
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
      ((effect con+ con- add del)
       (iter (for c in ground-conditions)
             (if (positive c)
                 (unless (static-p c)
                   (linear-extend con+ (strips.lib:index index c)))
                 (let ((i (strips.lib:index index (second c))))
                   (when i ; otherwise unreachable
                     (linear-extend con- (strips.lib:index index c))))))
       (when (positive atom)
         (strips.lib:query-trie
          (lambda (c) (linear-extend add (strips.lib:index index c)))
          trie atom))
       (when (negative atom)
         (strips.lib:query-trie
          (lambda (c) (linear-extend del (strips.lib:index index c)))
          trie (second atom)))))
    (linear-extend effects e)))
