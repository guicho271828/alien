(in-package :strips)

(defun run (search-engine)
  (match search-engine
    ((searcher storage body)
     (eval `(define-packed-struct state-information (,storage)))
     (asdf:compile-system :strips.instance-dependent :force t)
     (asdf:load-system :strips.instance-dependent :force t)
     ;; ensure the specialised code is removed and does not affect the later debugging
     (asdf:clear-system :strips.instance-dependent)
     (funcall (compile nil `(lambda () ,body))))))
