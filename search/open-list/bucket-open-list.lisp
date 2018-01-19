(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

;; slow, initial implementation of bucket open list as a placeholder for initial submission

(defstruct bucket-open-list
  "bucket open list with single key, fifo tiebreaking; initial, slow implementation"
  (min-key 0 :type fixnum)
  (buckets (make-array 32 :element-type 'list :initial-element nil :adjustable t)))

(defun bucket-open-list-insert (open key element)
  (match open
    ((bucket-open-list buckets :min-key (place min-key))
     (unless (array-in-bounds-p buckets key)
       (adjust-array buckets (max (length buckets) (expt 2 (integer-length key)))))
     (push element (aref buckets key))
     (minf min-key key))))

(defun bucket-open-list-pop (open)
  (ematch open
    ((bucket-open-list buckets :min-key (place min-key-place min-key))
     (prog1 (pop (aref buckets min-key))
       (do ()
           (nil)
         (incf min-key)
         (if (array-in-bounds-p buckets min-key)
             (when (aref buckets min-key)
               (setf min-key-place min-key)
               (return))
             (progn
               (setf min-key-place 0)
               (return))))))))
