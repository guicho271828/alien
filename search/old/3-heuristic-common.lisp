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

(deftype g-list ()
  '(array state-id))

(ftype* make-g-list g-list)
(defun make-g-list ()
  (make-a-array 1024 :element-type 'state-id :initial-element #xffffffff))
