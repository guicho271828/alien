
(in-package :pddl)

(cl:defmacro define (name cl:&body body)
  (cl:declare (cl:ignore name body))
  (cl:error "This is a dummy macro for editor integration"))

(cl:in-package :strips)

(named-readtables:in-readtable :fare-quasiquote)

(defun parse-typed-def (list)
  "Parse [objs* - type]* list. Does not handle the type inheritance.
 Returns an alist of parameters and their types.
 Untyped parameters are given the type OBJECT."
  (let (db
        buffer)
    (labels ((add (s type)
               (push (cons s type) db))
             (rec (list)
               (match list
                 ((list* '- type rest)
                  (dolist (s (nreverse buffer))
                    (add s type))
                  (setf buffer nil)
                  (rec rest))
                 
                 (nil
                  (dolist (s buffer)
                    (add s 'object))
                  (nreverse db))
                 
                 ((list* now rest)
                  (push now buffer)
                  (rec rest)))))
      (rec list))))


(print
 (parse-typed-def '(hand level beverage dispenser container - object
                    ingredient cocktail - beverage
                    shot shaker - container)))

(defun flatten-typed-def (typed-def)
  "Take a single typed predicate literal L and returns two values:
the untyped version of L and a list of literals converted from the types of the parameters.

 Example: (?x - table) -> (?x), ((table ?x)) "
  (let* ((parsed (parse-typed-def typed-def))
         (w/o-type (mapcar #'car parsed))
         (type-conditions
          (iter (for (arg . type) in parsed)
                (unless (eq type 'object)
                  (collect `(,type ,arg))))))
    (values w/o-type type-conditions)))

(defun flatten-types/condition (condition)
  (ematch condition
    ((list 'exists params condition)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(exists ,w/o-type
               (and ,@type-conditions
                    ,(flatten-types/condition condition)))))
    ((list 'forall params condition)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(forall ,w/o-type
                (imply (and ,@type-conditions)
                       ,(flatten-types/condition condition)))))
    ((list* (and kind (or 'and 'or))
            conditions)
     `(,kind ,@(mapcar #'flatten-types/condition conditions)))
    (_ condition)))

(print
 (flatten-types/condition `(forall (?u - unit) (and (clean)))))

(defun flatten-types/effect (effect)
  (ematch effect
    ((list* (and kind (or 'or 'exists)) _)
     (error "~a should not appear in the effects: ~a" kind effect))
    (`(forall ,params ,effect)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(forall ,w/o-type
                (when (and ,@type-conditions)
                  ,(flatten-types/effect effect)))))
    (`(when ,condition ,body)
      `(when ,(flatten-types/condition condition)
         ,(flatten-types/effect body)))
    (`(and ,@conditions)
     `(and ,@(mapcar #'flatten-types/effect conditions)))
    (_ effect)))



(defun find-domain (problem-path)
  (format t "~&finding the domain file...~%")
  (block nil
     (let ((dpath (make-pathname :defaults problem-path :name "domain")))
       (when (probe-file dpath) (return dpath)))
     (let ((dpath (make-pathname :defaults problem-path :name
                                 (format nil "~a-domain" (pathname-name problem-path)))))
       (when (probe-file dpath) (return dpath)))
     (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
            problem-path
            (make-pathname :defaults problem-path :name "domain")
            (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path))))))
