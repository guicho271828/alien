
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips.lib:define-packed-struct blind ()
  (blind 0 (integer 0 0)))

(defun blind (state)
  (declare (ignore state))
  0)
