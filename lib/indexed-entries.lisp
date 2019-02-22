
#|

A storage that allows fast bidirectional access to/from an index from/to an object.

For object -> index:  The object is looked up in a hash table to obtain the index, O(1).
For index  -> object: The object is looked up in an array, O(1).

|#

(in-package :alien.lib)

;; general indexed entry.

(defstruct (index (:constructor make-index (&key (test 'eql) &aux (hash (make-hash-table :test test)))))
  (array (make-array 32 :adjustable t :fill-pointer 0) :type array :read-only t)
  (hash (make-hash-table) :type hash-table :read-only t))

(ftype* index-insert index t (values array-index boolean))
(defun index-insert (index element)
  "insert the element to the index, return the id and a boolean indicating whether the element was insterted."
  (let ((a (index-array index))
        (h (index-hash index)))
    (if-let ((index (gethash element h)))
      index
      (multiple-value-prog1 (values (setf (gethash element h) (fill-pointer a)) t)
        (vector-push-extend element a (array-total-size a))))))

(ftype* index-id index t (values (or array-index null) boolean))
(defun index-id (index element)
  "return the id of the given element, and a boolean if the element exists (for telling NIL being registered or not)."
  (gethash element (index-hash index)))

(ftype* index-ref index array-index t)
(defun index-ref (index id)
  "return the element of the given id."
  (aref (index-array index) id))

(ftype* index-size index array-index)
(defun index-size (index)
  "return the number of elements in the index."
  (length (index-array index)))
