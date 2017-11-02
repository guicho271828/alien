
(in-package :pddl)

(cl:defmacro define (name cl:&body body)
  (cl:declare (cl:ignore name body))
  (cl:error "This is a dummy macro for editor integration"))

(cl:in-package :strips)


(defun parse-typed-def (list)
  "Parse [objs* - type]* list. Does not handle type inheritance"
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

(defun flatten-types (condition)
  (ematch condition
    ((list (and kind (or 'exists 'forall)) params condition)
     (let* ((parsed (parse-typed-def params))
            (w/o-type (mapcar #'car parsed))
            (type-conditions (mapcar (lambda-match ((cons arg type) `(,type ,arg))) parsed)))
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
