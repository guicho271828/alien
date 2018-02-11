
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

;; (in-package :sb-c)
;; 
;; (defknown subseq (sequence index &optional sequence-end) consed-sequence
;;     (flushable dx-safe))
;; 
;; (defoptimizer (subseq derive-type) ((sequence start &optional end) node)
;;   (let* ((sequence-type (lvar-type sequence))
;;          (constant-start (and (constant-lvar-p start)
;;                               (lvar-value start)))
;;          (constant-end (and (constant-lvar-p end)
;;                             (lvar-value end)))
;;          (index-length (and constant-start constant-end
;;                             (- constant-end constant-start)))
;;          (list-type (specifier-type 'list)))
;;     (flet ((bad ()
;;              (let ((*compiler-error-context* node))
;;                (compiler-warn "Bad bounding indices ~s, ~s for ~
;;                                ~/sb!impl:print-type/"
;;                               constant-start constant-end sequence-type))))
;;       (cond ((and index-length
;;                   (minusp index-length))
;;              ;; Would be a good idea to transform to something like
;;              ;; %compile-time-type-error
;;              (bad))
;;             ((csubtypep sequence-type list-type)
;;              (let ((null-type (specifier-type 'null)))
;;                (cond ((csubtypep sequence-type null-type)
;;                       (cond ((or (and constant-start
;;                                       (plusp constant-start))
;;                                  (and index-length
;;                                       (plusp index-length)))
;;                              (bad))
;;                             ((eql constant-start 0)
;;                              null-type)
;;                             (t
;;                              list-type)))
;;                      ((not index-length)
;;                       list-type)
;;                      ((zerop index-length)
;;                       null-type)
;;                      (t
;;                       (specifier-type 'cons)))))
;;             ((csubtypep sequence-type (specifier-type 'vector))
;;              (let* ((dimensions
;;                       ;; Can't trust lengths from non-simple vectors due to
;;                       ;; fill-pointer and adjust-array
;;                       (and (csubtypep sequence-type (specifier-type 'simple-array))
;;                            (ctype-array-dimensions sequence-type)))
;;                     (dimensions-length
;;                       (and (singleton-p dimensions)
;;                            (integerp (car dimensions))
;;                            (car dimensions)))
;;                     (length (cond (index-length)
;;                                   ((and dimensions-length
;;                                         (not end)
;;                                         constant-start)
;;                                    (- dimensions-length constant-start))))
;;                     (simplified (simplify-vector-type sequence-type)))
;;                (cond ((and dimensions-length
;;                            (or
;;                             (and constant-start
;;                                  (> constant-start dimensions-length))
;;                             (and constant-end
;;                                  (> constant-end dimensions-length))))
;;                       (bad))
;;                      (length
;;                       (type-intersection simplified
;;                                          (specifier-type `(simple-array * (,length)))))
;;                      (t
;;                       simplified))))
;;             ((not index-length)
;;              nil)
;;             ((zerop index-length)
;;              (specifier-type '(not cons)))
;;             (t
;;              (specifier-type '(not null)))))))
;; 
;; (deftransform subseq ((seq start &optional end)
;;                       (vector t &optional t)
;;                       *
;;                       :node node)
;;   (let ((type (lvar-type seq)))
;;     (cond
;;       ((and (inlineable-copy-vector-p type)
;;             (policy node (> speed space)))
;;        (let ((element-type (type-specifier (array-type-specialized-element-type type))))
;;          `(let* ((length (length seq))
;;                  (end (or end length)))
;;             ,(unless (policy node (zerop insert-array-bounds-checks))
;;                      '(progn
;;                        (unless (<= 0 start end length)
;;                          (sequence-bounding-indices-bad-error seq start end))))
;;             (let* ((size (- end start))
;;                    (result (make-array size :element-type ',element-type)))
;;               ,(maybe-expand-copy-loop-inline 'seq (if (constant-lvar-p start)
;;                                                        (lvar-value start)
;;                                                        'start)
;;                                               'result 0 'size element-type)
;;               result))))
;;       (t
;;        '(vector-subseq* seq start end)))))
;; 
;; (deftransform subseq ((seq start &optional end)
;;                       (list t &optional t))
;;   `(list-subseq* seq start end))
;; 
;; (deftransform subseq ((seq start &optional end)
;;                       ((and sequence (not vector) (not list)) t &optional t))
;;   '(sb!sequence:subseq seq start end))
;; 
;; (defoptimizer (subseq stack-allocate-result)
;;     ((sequence start &optional end) node dx)
;;   (declare (ignore dx sequence start end))
;;   t)
;; 
;; (in-package :strips.lib)

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



;; (SB-KERNEL:%VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; (SB-KERNEL:%set-VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; sb-vm::single-float-bits
;; sb-kernel:single-float-bits
;; sb-ext:single-float-negative-infinity 
;; sb-kernel:make-single-float
;; sb-kernel:make-double-float
