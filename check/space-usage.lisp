(in-package :strips)

;; as you can see below, sbcl is not compacting the slots

(defparameter *a* nil)
  
(defstruct a0)
(with-memory-usage-diff () (push (make-a0) *a*)) ; 16 bytes, 2 words
(defstruct a1 s1)
(with-memory-usage-diff () (push (make-a1) *a*)) ; 16 bytes, 2 words
(defstruct a2 s1 s2)
(with-memory-usage-diff () (push (make-a2) *a*)) ; 32 bytes, 4 words
(defstruct a3 s1 s2 s3)
(with-memory-usage-diff () (push (make-a3) *a*)) ; 32 bytes, 4 words
(defstruct a4 s1 s2 s3 s4)
(with-memory-usage-diff () (push (make-a4) *a*)) ; 48 bytes, 6 words

(defstruct (v0 (:type vector)))
(with-memory-usage-diff () (push (make-v0) *a*)) ; 16 bytes, 2 words
(defstruct (v1 (:type vector)) s1)
(with-memory-usage-diff () (push (make-v1) *a*)) ; 32 bytes, 4 words
(defstruct (v2 (:type vector)) s1 s2)
(with-memory-usage-diff () (push (make-v2) *a*)) ; 32 bytes, 4 words
(defstruct (v3 (:type vector)) s1 s2 s3)
(with-memory-usage-diff () (push (make-v3) *a*)) ; 48 bytes, 6 words
(defstruct (v4 (:type vector)) s1 s2 s3 s4)
(with-memory-usage-diff () (push (make-v4) *a*)) ; 48 bytes, 6 words

(defstruct (f0 (:type vector)))
(with-memory-usage-diff () (push (make-f0) *a*)) ; 16 bytes, 2 words
(defstruct (f1 (:type vector)) (s1 0 :type fixnum))
(with-memory-usage-diff () (push (make-f1) *a*)) ; 32 bytes, 4 words
(defstruct (f2 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum))
(with-memory-usage-diff () (push (make-f2) *a*)) ; 32 bytes, 4 words
(defstruct (f3 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum) (s3 0 :type fixnum))
(with-memory-usage-diff () (push (make-f3) *a*)) ; 48 bytes, 6 words
(defstruct (f4 (:type vector)) (s1 0 :type fixnum) (s2 0 :type fixnum) (s3 0 :type fixnum) (s4 0 :type fixnum))
(with-memory-usage-diff () (push (make-f4) *a*)) ; 48 bytes, 6 words


(defstruct (ub8v-0 (:type vector)))
(with-memory-usage-diff () (push (make-ub8v-0) *a*)) ; 16 bytes, 2 words
(defstruct (ub8v-1 (:type vector)) (s1 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8v-1) *a*)) ; 32 bytes, 4 words
(defstruct (ub8v-2 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8v-2) *a*)) ; 32 bytes, 4 words
(defstruct (ub8v-3 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8v-3) *a*)) ; 48 bytes, 6 words
(defstruct (ub8v-4 (:type vector)) (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)) (s4 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8v-4) *a*)) ; 48 bytes, 6 words

(defstruct ub8s-0)
(with-memory-usage-diff () (push (make-ub8s-0) *a*)) ; 16 bytes, 2 words
(defstruct ub8s-1 (s1 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8s-1) *a*)) ; 32 bytes, 4 words
(defstruct ub8s-2 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8s-2) *a*)) ; 32 bytes, 4 words
(defstruct ub8s-3 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8s-3) *a*)) ; 48 bytes, 6 words
(defstruct ub8s-4 (s1 0 :type (unsigned-byte 8)) (s2 0 :type (unsigned-byte 8)) (s3 0 :type (unsigned-byte 8)) (s4 0 :type (unsigned-byte 8)))
(with-memory-usage-diff () (push (make-ub8s-4) *a*)) ; 48 bytes, 6 words

(defstruct ub4s-0)
(with-memory-usage-diff () (push (make-ub4s-0) *a*)) ; 16 bytes, 2 words
(defstruct ub4s-1 (s1 0 :type (unsigned-byte 4)))
(with-memory-usage-diff () (push (make-ub4s-1) *a*)) ; 32 bytes, 4 words
(defstruct ub4s-2 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)))
(with-memory-usage-diff () (push (make-ub4s-2) *a*)) ; 32 bytes, 4 words
(defstruct ub4s-3 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)) (s3 0 :type (unsigned-byte 4)))
(with-memory-usage-diff () (push (make-ub4s-3) *a*)) ; 48 bytes, 6 words
(defstruct ub4s-4 (s1 0 :type (unsigned-byte 4)) (s2 0 :type (unsigned-byte 4)) (s3 0 :type (unsigned-byte 4)) (s4 0 :type (unsigned-byte 4)))
(with-memory-usage-diff () (push (make-ub4s-4) *a*)) ; 48 bytes, 6 words

;; a single bit vector consumes 32 bytes for tags etc.
(iter (for i below 16)
      (print (expt 2 i))
      (setf *a* (with-memory-usage-diff ()
                  (make-array (expt 2 i) :element-type 'bit))))

stack allocated alien example
(cffi:with-foreign-object (array :int 10)
  )
