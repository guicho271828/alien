(in-package :strips)

(defun run (search-engine)
  (match search-engine
    ((searcher storage form)
     (eval `(define-packed-struct state-information (,storage)))
     (funcall (compile nil form)))))
