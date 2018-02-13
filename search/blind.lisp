
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips.lib:define-packed-struct blind ())

(defun blind-heuristics (state)
  (declare (ignore state))
  0)

(defun blind ()
  (make-evaluator
   :storage 'blind
   :function 'blind-heuristics))
