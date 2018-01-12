(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

(defvar *open-list*)




(defstruct bucket-open-list
  (buckets (make-array 32 :element-type '(array))))

