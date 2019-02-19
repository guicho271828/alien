
(in-package :alien)
(named-readtables:in-readtable :fare-quasiquote)

(alien.lib:define-packed-struct blind ()
  (value 0 (integer 0 0)))

(defun blind-heuristics (state)
  (declare (ignore state))
  0)

(defun blind ()
  (make-evaluator
   :storage '(list 'blind)
   :function '(function blind-heuristics)))
