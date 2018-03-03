(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

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

