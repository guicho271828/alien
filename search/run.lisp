(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun run (search-engine)
  (destructuring-bind (storage body) search-engine
    (eval `(define-packed-struct state-information (,storage)))
    (funcall (compile nil `(lambda () ,body)))))

