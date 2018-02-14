(in-package :strips)

(defun run (search-engine)
  (match search-engine
    ((searcher storage form)
     (eval `(strips.lib:define-packed-struct state-information (,storage)))
     (funcall (compile nil form)))))
