(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; (cffi:defcstruct information
;;   (g :uint32)
;;   (visited :uchar)
;;   (visited :uchar))
  
;; (defstruct (information
;;              (:constructor make-information
;;                            (&key (element-type 'fixnum)
;;                                  (initial-element 0))))
;;   (array (make-a-array 32 :element-type element-type
;;                        :initial-element initial-element)
;;          :type array))
;; 
;; (defun information (info id)
;;   (match info
;;     ((information array)
;;      (if (array-in-bounds-p array id)
;;          (aref array id)
;;          (progn
;;            (adjust-array array (expt 2 (integer-length id)))
;;            (setf (aref array id) (copy-structure (aref array id))))))))

 
