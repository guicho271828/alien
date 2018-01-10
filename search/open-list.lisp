(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

(defvar *open-list*)

;; id-based, bucket based (because unit cost)

(defun maximum-state-id ()
  (floor (* 8 1024 *memory-limit*)
         *fact-size*))

;; (deftype state-id ()
;;   `(integer 0 ,(maximum-state-id)))

(defstruct bucket-open-list
  )

