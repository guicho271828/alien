(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

(defvar *open-list*)

(defstruct bucket-open-list
  "bucket open list with single key, fifo tiebreaking; initial, slow implementation"
  (min-key 0 :type fixnum)
  (buckets (make-array 32 :element-type 'list :initial-element nil :adjustable t)))

(defun bucket-open-list-insert (open key element)
  (match open
    ((bucket-open-list buckets :min-key (place min-key))
     (unless (array-in-bounds-p buckets key)
       (adjust-array buckets (* 2 (array-total-size buckets))))
     (push element (aref buckets key))
     (minf min-key key))))

(defun bucket-open-list-pop (open)
  (match open
    ((bucket-open-list buckets :min-key (place min-key-place min-key))
     (prog1 (pop (aref buckets min-key))
       (do ()
           ((aref buckets min-key)
            (setf min-key-place min-key))
         (incf min-key))))))
