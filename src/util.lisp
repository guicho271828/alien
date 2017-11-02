
(in-package :pddl)

(cl:defmacro define (name cl:&body body)
  (cl:declare (cl:ignore name body))
  (cl:error "This is a dummy macro for editor integration"))

(cl:in-package :strips)


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

(defun flatten-types (condition)
  (ematch condition
    ((list (and kind (or 'exists 'forall)) params condition)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(,kind ,w/o-type
               (and ,@type-conditions
                    ,(flatten-types condition)))))
    ((list* (and kind (or 'and 'or))
            conditions)
     `(,kind ,@(mapcar #'flatten-types conditions)))
    (_ condition)))

(print
 (flatten-types `(forall (?u - unit) (and (clean)))))

(defun find-domain (problem-path)
  (format t "~&finding the domain file...")
  (print
   (block nil
     (let ((dpath (make-pathname :defaults problem-path :name "domain")))
       (when (probe-file dpath) (return dpath)))
     (let ((dpath (make-pathname :defaults problem-path :name
                                 (format nil "~a-domain" (pathname-name problem-path)))))
       (when (probe-file dpath) (return dpath)))
     (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
            problem-path
            (make-pathname :defaults problem-path :name "domain")
            (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path)))))))
