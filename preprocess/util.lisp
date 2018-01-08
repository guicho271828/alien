
(in-package :pddl)

(cl:defmacro define (name cl:&body body)
  (cl:declare (cl:ignore name body))
  (cl:error "This is a dummy macro for editor integration"))

(cl:in-package :strips)

(defmacro defun* (name args &body body)
  (match name
    ((or (and (symbol) name       (<> return-type '*))
         (and (list 'setf _) name (<> return-type '*))
         (and (list name return-type))
         (and (list (and (list 'setf _) name) return-type)))
     (let ((required (or (position-if (lambda (elem) (member elem '(&optional &key &rest &aux))) args)
                         (length args))))
       (iter (for (arg . rest) on args)
             (repeat required)
             (match arg
               ((list arg type)
                (collecting arg into argsyms)
                (collecting type into types))
               (_
                (collecting arg into argsyms)
                (collecting '* into types)))
             (finally
              (let* ((rest-args (nthcdr required args))
                     (rest-types (iter (for elem in rest-args)
                                       (collecting
                                        (if (member elem '(&optional &key &rest &aux))
                                            elem
                                            '*)))))
                (return
                  `(progn
                     (declaim (ftype (function (,@types ,@rest-types) ,return-type) ,name))
                     (defun ,name ,(append argsyms rest-args)
                       ,@body))))))))))

#+(or)
(progn
  (defun* fn (a b c &rest rest)
    (list (+ a b c) rest))
  (defun* (fn list) (a b c &rest rest)
    (list (+ a b c) rest))
  (defun* (fn list) ((a fixnum) b c &rest rest)
    (list (+ a b c) rest)))

(defun %rel (pathname)
  (asdf:system-relative-pathname :strips pathname))

(defmacro print-values (&body form)
  `(multiple-value-call (lambda (&rest args) (mapcar #'print args))
     ,@form))

(defmacro errors (&body form)
  `(handler-case
       (progn ,@form)
     (error (c)
       (format t "~&this is an error:~% ~a~%" c))))

(defmacro with-parsed-information (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

(defmacro with-parsed-information2 (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :monotonicity *monotonicity*
             :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

(defmacro with-parsed-information3 (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :facts *facts*
             :ops *ops*
             :ground-axioms *ground-axioms*
             :monotonicity *monotonicity*
             :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

(defmacro with-parsed-information4 (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :axiom-layers *axiom-layers*
             :facts *facts*
             :ops *ops*
             :ground-axioms *ground-axioms*
             :monotonicity *monotonicity*
             :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

(defmacro with-parsed-information5 (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :fact-index *fact-index*
             :fact-size *fact-size*
             :fact-trie *fact-trie*
             :op-index *op-index*
             :instantiated-ops *instantiated-ops*

             :axiom-layers *axiom-layers*
             :facts *facts*
             :ops *ops*
             
             :ground-axioms *ground-axioms*
             :monotonicity *monotonicity*
             :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

(defun positive (form)
  ;; (declare (optimize (speed 3) (safety 0)))
  (not (member (car form) '(not increase)))
  ;; (ematch form
  ;;   ((list* (or 'not 'increase) _)
  ;;    nil)
  ;;   (_
  ;;    ;; (list* name  _)
  ;;    ;; (assert (member name *predicates* :key #'first))
  ;;    t))
  )

(defun negative (form)
  (ematch form
    ((list* 'not _)
     t)
    ((list* _)
     nil)))

(defun ensure-zeroary-to-atom (f)
  "convert a nullary predicate to a prolog atom, e.g. (goal) -> goal"
  (match f
    ((list x) x)
    (_ f)))

(defun ensure-zeroary-to-list (f)
  "convert a nullary predicate to a prolog list, e.g. (goal) -> (list goal)"
  (match f
    ((list x) `(list ,x))
    (_ f)))

(declaim (inline make-bit-vector))
(defun make-bit-vector (length)
  (make-array length :element-type 'bit :initial-element 0))

(declaim (inline linear-extend))
(defun linear-extend (vector element)
  (vector-push-extend element vector (array-total-size vector)))

(defmacro in-compile-time ((environment) &body body &environment env)
  (check-type environment symbol)
  (eval `(let ((,environment ,env)) (progn ,@body))))
