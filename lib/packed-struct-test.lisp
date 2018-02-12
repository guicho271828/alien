
(in-package :strips.lib)

;; test

(deftype scalar () '(unsigned-byte 16))

(deftype parent () '(unsigned-byte 20))

(deftype generator () '(unsigned-byte 8))

(deftype status () '(member 0 1 2 3))

(define-packed-struct test1 () ; 64bit
  (scalar 0 scalar)
  (parent 0 parent)
  (generator 0 generator)
  (status 0 status))

#+(or)
(define-packed-struct test2 ; should fail, duplicated slot names
  (scalar 0 scalar)
  (scalar 0 scalar))

(define-packed-struct state-info ()
  (state 0 (bit-vector 42))
  (status +new+ status)
  (parent 0 parent)
  (generator 0 generator))

(define-packed-struct g ()
  (g 0 scalar))

(print
 (merge-packed-struct-layout '(state-info g)
                             :name 'state-info+g))

(print
 (size-of (merge-packed-struct-layout '(state-info g)
                                      :name 'state-info+g)))

(define-packed-struct state-info+g (state-info g))

(defvar *state-info* (make-state-info+g-array 1000))
(describe *state-info*)

(defun println (x)
  (write x :escape nil) (terpri))

(defmacro print-values (&body form)
  `(multiple-value-call (lambda (&rest args) (map nil #'println args))
     ,@form))

(defmacro in-compile-time ((environment) &body body &environment env)
  (check-type environment symbol)
  (eval `(let ((,environment ,env)) (progn ,@body))))

(defun test-packed-aref ()
  (declare (optimize (speed 3))
           (notinline make-state-info+g-array))
  (let ((state-info (make-state-info+g-array 1000)))
    (declare ((simple-bit-vector 88000) state-info))
    (in-compile-time (env)
      (print-values (sb-cltl2:variable-information 'state-info env)))
    (let ((elem (packed-aref state-info 'state-info+g 500))
          (b1 (make-array 100 :element-type 'bit))
          (b2 (let ((begin793 (* 500 88)))
                (subseq state-info begin793 (+ begin793 88))))
          (b3 (subseq state-info 44000 44088))
          (b4 (let ((r (make-array 88 :element-type 'bit)))
                (replace r state-info :start2 44000 :end2 44088)
                r)))
      (declare (sb-int:truly-dynamic-extent elem b1 b2 b3 b4))
      (in-compile-time (env)
        (print-values (sb-cltl2:variable-information 'elem env)))
      (print elem)
      (print b1)
      (print b2)
      (print b3)
      1)))

(defun test-dx (b)
  (declare (optimize (speed 3))
           ((simple-bit-vector 4000) b))
  (let ((b1 (subseq b 0 100))
        (b2 (let ((r (make-array 100 :element-type 'bit)))
              (replace r b :start2 0 :end2 100)
              r))
        (b3 (make-array 100 :element-type 'bit))
        (b4 (let ((r (make-array 100 :element-type 'bit)))
              (setf (aref r 0) 1)
              r))
        (b5 (replace (make-array 100 :element-type 'bit) b :start2 0 :end2 100)))
    (declare (sb-int:truly-dynamic-extent b1 b2 b3 b4 b5))
    ;; (declare (dynamic-extent b1 b2 b3 b4))
    (replace b3 b :start2 0 :end2 100)
    (print b1)
    (print b2)
    (print b3)
    (print b4)
    (print b5)
    nil))

;; constant folded
(size-of 'state-info)

(defun test-packed-ref ()
  (declare (optimize (speed 3))
           (notinline make-state-info+g-array))
  (let ((state-info (make-state-info+g-array 1000)))
    (print state-info)
    (let ((ptr (packed-ref 'state-info+g 500))
          (instance (make-state-info+g)))
      (declare (dynamic-extent instance))
      (replace instance state-info :start2 ptr)
      (print instance)
      (print ptr))))

;; (SB-KERNEL:%VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; (SB-KERNEL:%set-VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; sb-vm::single-float-bits
;; sb-kernel:single-float-bits
;; sb-ext:single-float-negative-infinity 
;; sb-kernel:make-single-float
;; sb-kernel:make-double-float
