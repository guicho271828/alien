

(in-package :strips.lib)

;; general indexed entry.

(defstruct (index (:constructor make-index (&key (test 'eql) &aux (hash (make-hash-table :test test)))))
  (array (make-array 32 :adjustable t :fill-pointer 0) :type array :read-only t)
  (hash (make-hash-table) :type hash-table :read-only t))

(defun index-insert (index element)
  (let ((a (index-array index))
        (h (index-hash index)))
    (unless (gethash element h)
      (setf (gethash element h) (fill-pointer a))
      (vector-push-extend element a (array-total-size a)))
    nil))

(defun index (index element)
  (gethash element (index-hash index)))

(defun index-ref (index i)
  (aref (index-array index) i))

(defun index-size (index)
  (length (index-array index)))
