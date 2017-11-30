
(in-package :strips)

;; X -> classid
;; classid -> [X]

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct equivalence
    (total    -1 :type fixnum)
    (obsolete -1 :type fixnum)
    (groups (make-array 32 :element-type 'list :initial-element nil :adjustable t :fill-pointer 0)
            :type (array list))
    (hash (make-hash-table) :type hash-table)))

(defun delete-class (ec class)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence :obsolete (place obsolete) :groups groups)
     (setf (aref groups class) nil)
     (incf obsolete)
     ;; maybe shrink the vector later
     )))

(defun class-id (ec x)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence hash :total (place total) :groups groups)
     (or (gethash x hash)
         (let ((class (incf total)))
           (setf (gethash x hash) class)
           (vector-push-extend (list x) groups class)
           class)))))

(defun add-relation (ec x y)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence hash :obsolete (place obsolete) :groups groups)
     (let ((xc (class-id ec x))
           (yc (class-id ec y)))
       (when (> xc yc)
         (rotatef x y)
         (rotatef xc yc))
       (dolist (y (aref groups yc))
         (setf (gethash y hash) yc))
       (nconcf (aref groups xc)
               (aref groups yc))
       (delete-class ec yc)))))



