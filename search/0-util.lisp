
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; decoding state

(defun decode-state (state)
  (iter (for b in-vector state with-index i)
        (when (= 1 b)
          (collect (strips.lib:index-ref *fact-index* i)))))

(defun decode-op (op)
  (ematch op
    ((integer)
     (match (strips.lib:index-ref *op-index* op)
       ((list (list* name args) _)
        (list* (getf (find name *actions* :key #'second) :original-action)
               args))))
    ((op)
     (decode-op (position op *instantiated-ops*)))))

(defmacro enumerate (name &body name-or-value)
  "Define a type NAME which could take some values specified by name-or-value.
name-or-value is a list whose element is whether a SYMBOL for a constant variable, or a list (SYMBOL INTEGER).
In the first form, the value of enumeration starts from 0 and increments in the each element.
In the second form, the value is set to INTEGER and the later elements increments from this value.
You can mix both forms. "
  (iter (with counter = 0)
        (for elem in name-or-value)
        (ematch elem
          ((symbol)
           (collecting `(define-constant ,elem ,counter) into body)
           (collecting counter into values)
           (incf counter))
          ((list (and name (symbol)) (and int (integer)))
           (collecting `(define-constant ,name ,int) into body)
           (setf counter int)
           (collecting counter into values)
           (incf counter)))
        (finally
         (return
           `(progn
              (deftype ,name () '(member ,@values))
              ,@body)))))


(defun memory-usage ()
  (sb-ext:gc :full t)
  (sb-vm:memory-usage :print-spaces t :count-spaces '(:dynamic) :print-summary nil))

(defmacro with-memory-usage-diff ((&key (spaces '(:dynamic))) &body body)
  `(call-with-memory-usage-diff ',spaces (lambda () ,@body)))

(defun call-with-memory-usage-diff (spaces fn)
  (sb-ext:gc :full t)
  (let ((old (mapcar #'sb-vm::type-breakdown spaces)))
    (prog1 (funcall fn)
      (sb-ext:gc :full t)
      (let ((new (mapcar #'sb-vm::type-breakdown spaces)))
        (iter (for old-breakdown in old)
              (for new-breakdown in new)
              (for space in spaces)
              (format t "~&Memory consumption in ~a:~%" space)
              (let ((diff (iter (for (byte count type) in new-breakdown)
                                (for (obyte ocount otype) = (find type old-breakdown :key #'third))
                                (unless otype
                                  (setf obyte 0 ocount 0))
                                (unless (zerop (- count ocount))
                                  (collecting (list (- byte obyte) (- count ocount) type))))))
                (iter (for (byte count type) in diff)
                      (with digits1 = (1+ (sb-ext:decimal-with-grouped-digits-width
                                           (reduce #'max (mapcar #'first diff)))))
                      (with digits2 = (1+ (sb-ext:decimal-with-grouped-digits-width
                                           (reduce #'max (mapcar #'second diff)))))
                      (format t "~v@:d bytes for ~v@:d ~a objects~%"
                              digits1 byte
                              digits2 count
                              type))))))))

;; as you can see below, sbcl is not compacting the slots

;; (defparameter *a* nil)
;;   
;; (defstruct a0)
;; (with-memory-usage-diff () (push (make-a0) *a*)) ; 16 bytes, 2 words
;; (defstruct a1 s1)
;; (with-memory-usage-diff () (push (make-a1) *a*)) ; 16 bytes, 2 words
;; (defstruct a2 s1 s2)
;; (with-memory-usage-diff () (push (make-a2) *a*)) ; 32 bytes, 4 words
;; (defstruct a3 s1 s2 s3)
;; (with-memory-usage-diff () (push (make-a3) *a*)) ; 32 bytes, 4 words
;; (defstruct a4 s1 s2 s3 s4)
;; (with-memory-usage-diff () (push (make-a4) *a*)) ; 48 bytes, 6 words
;; 
;; (defstruct (v0 (:type vector)))
;; (with-memory-usage-diff () (push (make-v0) *a*)) ; 16 bytes, 2 words
;; (defstruct (v1 (:type vector)) s1)
;; (with-memory-usage-diff () (push (make-v1) *a*)) ; 32 bytes, 4 words
;; (defstruct (v2 (:type vector)) s1 s2)
;; (with-memory-usage-diff () (push (make-v2) *a*)) ; 32 bytes, 4 words
;; (defstruct (v3 (:type vector)) s1 s2 s3)
;; (with-memory-usage-diff () (push (make-v3) *a*)) ; 48 bytes, 6 words
;; (defstruct (v4 (:type vector)) s1 s2 s3 s4)
;; (with-memory-usage-diff () (push (make-v4) *a*)) ; 48 bytes, 6 words
;; 
;; (defstruct (f0 (:type vector)))
;; (with-memory-usage-diff () (push (make-f0) *a*)) ; 16 bytes, 2 words
;; (defstruct (f1 (:type vector)) (s1 0 :type fixnum))
;; (with-memory-usage-diff () (push (make-f1) *a*)) ; 32 bytes, 4 words
;; (defstruct (f2 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum))
;; (with-memory-usage-diff () (push (make-f2) *a*)) ; 32 bytes, 4 words
;; (defstruct (f3 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum) (s3 0 :type fixnum))
;; (with-memory-usage-diff () (push (make-f3) *a*)) ; 48 bytes, 6 words
;; (defstruct (f4 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum) (s3 0 :type fixnum) (s4 0 :type fixnum))
;; (with-memory-usage-diff () (push (make-f4) *a*)) ; 48 bytes, 6 words
;; 
;; 
;; (defstruct (ub8v-0 (:type vector)))
;; (with-memory-usage-diff () (push (make-ub8v-0) *a*)) ; 16 bytes, 2 words
;; (defstruct (ub8v-1 (:type vector)) (s1 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8v-1) *a*)) ; 32 bytes, 4 words
;; (defstruct (ub8v-2 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8v-2) *a*)) ; 32 bytes, 4 words
;; (defstruct (ub8v-3 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8v-3) *a*)) ; 48 bytes, 6 words
;; (defstruct (ub8v-4 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)) (s4 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8v-4) *a*)) ; 48 bytes, 6 words
;; 
;; (defstruct ub8s-0)
;; (with-memory-usage-diff () (push (make-ub8s-0) *a*)) ; 16 bytes, 2 words
;; (defstruct ub8s-1 (s1 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8s-1) *a*)) ; 32 bytes, 4 words
;; (defstruct ub8s-2 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8s-2) *a*)) ; 32 bytes, 4 words
;; (defstruct ub8s-3 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8s-3) *a*)) ; 48 bytes, 6 words
;; (defstruct ub8s-4 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)) (s4 0 :type (unsigned-byte 8)))
;; (with-memory-usage-diff () (push (make-ub8s-4) *a*)) ; 48 bytes, 6 words
;; 
;; (defstruct ub4s-0)
;; (with-memory-usage-diff () (push (make-ub4s-0) *a*)) ; 16 bytes, 2 words
;; (defstruct ub4s-1 (s1 0 :type (unsigned-byte 4)))
;; (with-memory-usage-diff () (push (make-ub4s-1) *a*)) ; 32 bytes, 4 words
;; (defstruct ub4s-2 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)))
;; (with-memory-usage-diff () (push (make-ub4s-2) *a*)) ; 32 bytes, 4 words
;; (defstruct ub4s-3 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)) (s3 0 :type (unsigned-byte 4)))
;; (with-memory-usage-diff () (push (make-ub4s-3) *a*)) ; 48 bytes, 6 words
;; (defstruct ub4s-4 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)) (s3 0 :type (unsigned-byte 4)) (s4 0 :type (unsigned-byte 4)))
;; (with-memory-usage-diff () (push (make-ub4s-4) *a*)) ; 48 bytes, 6 words

;; a single bit vector consumes 32 bytes for tags etc.
;; (iter (for i below 16) (print (expt 2 i)) (setf *a* (with-memory-usage-diff () (make-array (expt 2 i) :element-type 'bit))))

;; stack allocated alien example
;; (cffi:with-foreign-object (array :int 10)
;;   )
