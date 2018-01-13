(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; heuristics

(deftype scalar ()
  "integer used to store g-value, h-value etc."
  `(unsigned-byte 16))

(deftype evaluator ()
  '(function (state-id) scalar))

(deftype heuristics ()
  '(function (state) scalar))
