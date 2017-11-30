
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

