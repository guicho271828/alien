
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(ftype* timeout searcher)
(defun timeout (searcher-fn)
  (bt:with-timeout (*time-limit*)
    (funcall searcher-fn))
  nil)

