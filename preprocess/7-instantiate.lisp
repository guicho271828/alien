(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *index*)
(defvar *fluent-size*)
(defvar *trie*)
(defvar *instantiated-ops*)

(defun instantiate (info)
  (with-parsed-information4 info
    (multiple-value-bind (index fluent-size trie) (index-facts)
      (list* :index index
             :fluent-size fluent-size
             :trie trie
             :instantiated-ops (instantiate-ops index trie)
             info))))

(defun index-facts ()
  (let ((i (strips.lib:make-index :test 'equal))
        (trie (strips.lib:make-trie))
        (fluent-size 0))
    ;; indexing init
    (dolist (f *init*)
      (when (deleted-p f)
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
  (mapcar (lambda (op) (instantiate-op op index trie)) *ops*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (effect (:constructor make-effect (size &aux
                                                     (con+ (make-bit-vector size))
                                                     (con- (make-bit-vector size))
                                                     (add (make-bit-vector size))
                                                     (del (make-bit-vector size)))))
    (con+ (make-bit-vector 0) :type (simple-bit-vector))
    (con- (make-bit-vector 0) :type (simple-bit-vector))
    (add (make-bit-vector 0) :type (simple-bit-vector))
    (del (make-bit-vector 0) :type (simple-bit-vector)))
  
  (defstruct (op (:constructor make-op (size &aux
                                             (pre+ (make-bit-vector size))
                                             (pre- (make-bit-vector size))
                                             (eff (make-array 32 :element-type 'effect :adjustable t :fill-pointer 0)))))
    (pre+ (make-bit-vector 0) :type (simple-bit-vector))
    (pre- (make-bit-vector 0) :type (simple-bit-vector))
    (eff (make-array 0 :elementy-type 'effect :adjustable t :fill-pointer 0) :type (array effect))))

(defun instantiate-op (op index trie)
  (ematch op
    (`((,name ,@args) ,_)
      (ematch (find name *actions* :key #'second)
        ((plist :parameters params
                :precondition `(and ,@precond)
                :effect `(and ,@effects))
         (let* ((gpre (copy-tree precond))
                (geff (copy-tree effects))
                (op (make-op (strips.lib:index-size index))))
           (iter (for a in args)
                 (for p in params)
                 (setf gpre (nsubst a p gpre))
                 (setf geff (nsubst a p geff)))

           (match op
             ((op pre+ pre- eff)
              (dolist (c gpre)
                (if (positive c)
                    (unless (static-p c)
                      (setf (aref pre+ (strips.lib:index index c)) 1))
                    (let ((i (strips.lib:index index (second c))))
                      (when i ; otherwise unreachable
                        (setf (aref pre- i) 1)))))
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
  (let ((e (make-effect (strips.lib:index-size index))))
    (match e
      ((effect con+ con- add del)
       (iter (for c in ground-conditions)
             (if (positive c)
                 (setf (aref con+ (strips.lib:index index c)) 1)
                 (let ((i (strips.lib:index index (second c))))
                   (when i ; otherwise unreachable
                     (setf (aref con- i) 1)))))
       (when (positive atom)
         (strips.lib:query-trie
          (lambda (c) (setf (aref add (strips.lib:index index c)) 1))
          trie atom))
       (when (negative atom)
         (strips.lib:query-trie
          (lambda (c)
            (setf (aref del (strips.lib:index index c)) 1))
          trie (second atom)))))
    (linear-extend effects e)))
