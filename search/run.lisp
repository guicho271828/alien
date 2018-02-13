(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun run (search-engine)
  (match search-engine
    ((searcher storage body)
     (eval `(define-packed-struct state-information (,storage)))
     (funcall (compile nil `(lambda () ,body))))))

