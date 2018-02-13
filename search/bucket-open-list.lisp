(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

;; slow, initial implementation of bucket open list as a placeholder for initial submission

(defstruct bucket-open-list
  "bucket open list with single key, fifo tiebreaking; initial, slow implementation"
  (min-key 32 :type array-index)
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
     (when (not (array-in-bounds-p buckets min-key))
       (error 'no-solution))
     (prog1 (pop (aref buckets min-key))
       (do ()
           ((if (array-in-bounds-p buckets min-key)
                (aref buckets min-key)
                t)
            (setf min-key-place min-key))
         (incf min-key))))))

(defun bucket-open-list (evaluator)
  (destructuring-bind (storage function) evaluator
    (list storage
          'make-bucket-open-list
          `(lambda (open element)
             (bucket-open-list-insert
              open
              (,function element)
              element))
          'bucket-open-list-pop)))
