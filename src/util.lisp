
(in-package :pddl)

(cl:defmacro define (name cl:&body body)
  (cl:declare (cl:ignore name body))
  (cl:error "This is a dummy macro for editor integration"))

(cl:in-package :strips)

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
