
(in-package :strips.lib)

;; I looked up various alternatives, but none satisfies my needs of storing and retrieving
;; certain data as compact as possible.

;; on SBCL:
;; Basic common lisp structure consumes additional 2 words for a single structure.
;; This multiplies the space usage when you make a huge number of small structures.
;; we need much more compact, C-like struct.

;; C-style struct

(lispn:define-namespace packed-struct-layout packed-struct-layout nil
                        "A namespace for the layout of the packed structure")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (packed-struct-layout (:constructor
                                    make-packed-struct-layout
                                    (&key
                                     name names defaults types
                                     &aux
                                     (sizes (mapcar #'size-of types))
                                     (offsets (compute-offset sizes))
                                     (aux (assert (equal names (remove-duplicates names)))))))
    (name nil :type symbol)
    names
    defaults
    types
    sizes
    offsets))


(defmethod make-load-form ((self packed-struct-layout) &optional environment)
  (make-load-form-saving-slots self
                               :environment environment))

(defun size-of (type)
  (match type
    ((packed-struct-layout sizes)
     (* 8 (ceiling (reduce #'+ sizes) 8)))
    (_
     (ematch (introspect-environment:typexpand type)
       ((type-r:integer-subtype low high)
        (if (minusp low)
            (1+ (integer-length high))
            (integer-length high)))
       ((type-r:float-subtype high)
        (multiple-value-bind (signif expon) (integer-decode-float high)
          (+ (integer-length signif)
             (integer-length expon)
             1)))
       ((type-r:member-type members)
        (size-of `(integer ,(reduce #'min members)
                           ,(reduce #'max members))))
       ((type-r:array-subtype element-type dimensions)
        (* (size-of element-type) (reduce #'* (ensure-list dimensions))))))))

(defun compute-offset (sizes)
  (iter (for s in sizes)
        (collecting sum)
        (summing s into sum)))

(defun instantiate-packed-struct (layout)
  (make-array (/ (size-of layout) 8) :element-type '(unsigned-byte 8)))

(defun merge-packed-struct-layout (structs &key (name (gensym)) names defaults types)
  (let ((result (reduce #'merge-packed-struct-layout-2
                        (mapcar #'symbol-packed-struct-layout structs)
                        :initial-value
                        (make-packed-struct-layout :name (gensym)
                                                   :names names
                                                   :defaults defaults
                                                   :types types))))
    (setf (packed-struct-layout-name result) name)
    result))

(defun merge-packed-struct-layout-2 (struct1 struct2)
  (ematch* (struct1 struct2)
    (((packed-struct-layout :names names1 :defaults defaults1 :types types1)
      (packed-struct-layout :names names2 :defaults defaults2 :types types2))
     (make-packed-struct-layout
      :name (gensym)
      :names    (append names1 names2)
      :defaults (append defaults1 defaults2)
      :types    (append types1 types2)))))

(defmacro define-packed-struct (name (&rest supertypes) &body slots)
  (let* ((slots (mapcar #'ensure-list slots))
         (layout (merge-packed-struct-layout
                  supertypes
                  :name name
                  :names (mapcar #'first slots)
                  :defaults (mapcar #'second slots)
                  :types (mapcar #'third slots))))
    (match layout
      ((packed-struct-layout names offsets sizes types)
       `(progn (setf (symbol-packed-struct-layout ',name)
                     ,layout)
               (defun ,(symbolicate 'make- name) ()
                 (make-array ,(/ (size-of layout) 8) :element-type '(unsigned-byte 8)))
               ,@(iter (for slotname in names)
                       (for offset in offsets)
                       (for size in sizes)
                       (for type in types)
                       (collecting
                        (%packed-accessor-lambda slotname offset size type))))
       name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; treating a big vector as a large integer and retrieve the value

(declaim (inline %packed-accessor-int))
(defun %packed-accessor-int (vector size position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           (simple-array vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (multiple-value-bind (index-end offset-end) (floor (+ size position) 64)
      (cond
        ((= index-begin index-end)
         (ldb (byte size offset-begin)
              (sb-kernel:%vector-raw-bits vector index-begin)))
        (t
         (+ (ldb (byte (- 64 offset-begin) offset-begin)
                 (sb-kernel:%vector-raw-bits vector index-begin))
            (ash (ldb (byte offset-end 0)
                      (sb-kernel:%vector-raw-bits vector index-end))
                 (- 64 offset-begin))))))))

(declaim (inline (setf %packed-accessor-int)))
(defun (setf %packed-accessor-int) (newval vector size position)
  "position: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           ((unsigned-byte 64) newval)
           (simple-array vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (multiple-value-bind (index-end offset-end) (floor (+ size position) 64)
      (cond
        ((= index-begin index-end)
         (setf (ldb (byte size offset-begin)
                    (sb-kernel:%vector-raw-bits vector index-begin))
               newval))
        (t
         (setf (ldb (byte (- 64 offset-begin) offset-begin)
                    (sb-kernel:%vector-raw-bits vector index-begin))
               (ldb (byte (- 64 offset-begin) 0)
                    newval)
               (ldb (byte offset-end 0)
                    (sb-kernel:%vector-raw-bits vector index-end))
               (ldb (byte offset-end (- 64 offset-begin))
                    newval)))))))

(defun %packed-accessor-test0 ()
  (print (SB-KERNEL:%VECTOR-RAW-BITS #*0000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000 0))
  (print (SB-KERNEL:%VECTOR-RAW-BITS #*0000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000 1))
  (print (SB-KERNEL:%VECTOR-RAW-BITS #*0000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000 2))
  (print (SB-KERNEL:%VECTOR-RAW-BITS #*0000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000 3)))

(defun %packed-accessor-test (vector)
  (declare (optimize (speed 3)))
  ;; (%packed-accessor-int vector 0 0)
  ;; (%packed-accessor-int vector 0 5)
  ;; (%packed-accessor-int vector 0 15)
  ;; (%packed-accessor-int vector 0 105)
  ;; (%packed-accessor-int vector 5 0)
  ;; (%packed-accessor-int vector 5 5)
  ;; (%packed-accessor-int vector 5 15)
  ;; (%packed-accessor-int vector 5 105)
  ;; (%packed-accessor-int vector 32 0)
  ;; (%packed-accessor-int vector 32 5)
  ;; (%packed-accessor-int vector 32 15)
  ;; (%packed-accessor-int vector 32 105)
  ;; (%packed-accessor-int vector 62 0)
  ;; (%packed-accessor-int vector 62 5)
  ;; (%packed-accessor-int vector 62 15)
  (%packed-accessor-int vector 62 105))

(defun %packed-accessor-test2 ()
  (declare (optimize (speed 3)))
  ;; length 64
  (let ((b #*0000000000000000000000000000000000000000000000000000000000000000))
    (setf (%packed-accessor-int b 4 0) #b1111)
    (print (%packed-accessor-int b 4 0))
    (setf (%packed-accessor-int b 4 4) #b11000) ; the fifth digit is out of bounds
    (print (%packed-accessor-int b 4 4))
    (print b))
  ;; length 128
  (let ((b #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
    (setf (%packed-accessor-int b 4 0) #b1111)
    (print (%packed-accessor-int b 4 0))
    (setf (%packed-accessor-int b 4 4) #b11000) ; the fifth digit is out of bounds
    (print (%packed-accessor-int b 4 4))
    (setf (%packed-accessor-int b 2 63) #b11) ; across word boundary
    (print (%packed-accessor-int b 2 63))
    (print (%packed-accessor-int b 1 63))
    (print (%packed-accessor-int b 1 64))
    (print b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (inline a))
;; (defun a (x)
;;   (print x))
;; (declaim (inline b))
;; (defun b (x)
;;   (a x))
;; (print (function-lambda-expression #'a))
;; (print (function-lambda-expression #'b))

;; sb-ext:*inline-expansion-limit*


;; (declaim (inline %packed-accessor-float))
;; (defun %packed-accessor-float32 (octet-vector offset size)
;;   (let ((int (%packed-accessor-int octet-vector offset size)))
;;     (let ((signif (float (dpb ))
;;           (exponent )
;;           (sign ))
;;       (scale-float
;;        (float (if (zerop sign)
;;                   signif
;;                   (- signif)))
;;         exponent)))))

#+(or)
(iter (for f in (map-product (lambda (&rest args) (apply #'symbolicate args))
                             '(most-positive
                               least-positive
                               least-positive-normalized
                               most-negative
                               least-negative
                               least-negative-normalized)
                             '(-)
                             '(single short double long)
                             '(-float)))
      (multiple-value-bind (signif expon sign) (integer-decode-float (eval f))
        (format t "~a~%" f)
        (format t "~d ~x ~o ~b~%" signif signif signif signif)
        (format t "~d ~x ~o ~b~%" expon expon expon expon)
        (format t "~d ~x ~o ~b~%" sign sign sign sign)))

#+(or)
(multiple-value-bind (signif exponent sign) (integer-decode-float -0.005)
  (print (list signif exponent sign))
  (print (scale-float
          (float
           (if (zerop sign)
               signif
               (- signif)))
          exponent)))

;; (* sign (scale-float signif exponent))

;; (declaim (inline %packed-accessor-bitvector))
;; (defun %packed-accessor-bitvector (octet-vector slotname offset size)
;;   (let* ((begin offset)
;;          (end   (+ size offset)))
;;     (multiple-value-bind (index-begin offset-begin) (floor begin 8)
;;       (multiple-value-bind (index-end offset-end) (floor end 8)
;;         ;; 5        6        7
;;         ;; 00000111 11111111 11100000
;;         ;;
;;         (labels ((rec (int i)
;;                    (if (= i index-end)
;;                        (+ (ash int offset-end)
;;                           (mask-field (byte 0 offset-end)
;;                                       (aref instance i)))
;;                        (rec (+ (ash int 8)
;;                                (aref instance i))
;;                             (1+ i)))))
;;           (declare (inline rec))
;;           (rec (mask-field (byte offset-begin 8)
;;                            (aref instance index-begin))
;;                (1+ index-begin)))))))
;; 

;; test

;; (deftype scalar () '(unsigned-byte 32))
;; 
;; (deftype parent () '(unsigned-byte 32))
;; 
;; (deftype generator () '(unsigned-byte 32))
;; 
;; (deftype status () '(member 0 1 2 3))
;; 
;; (define-packed-struct test1 ()
;;   (scalar 0 scalar)
;;   (parent 0 parent)
;;   (generator 0 generator))
;; 
;; #+(or)
;; (define-packed-struct test2 ; should fail, duplicated slot names
;;   (scalar 0 scalar)
;;   (scalar 0 scalar))
;; 
;; (define-packed-struct state-info ()
;;   (state 0 (bit-vector 42))
;;   (status +new+ status)
;;   (parent 0 parent)
;;   (generator 0 generator))
;; 
;; (define-packed-struct g ()
;;   (g 0 scalar))
;; 
;; (merge-packed-struct-layout '(state-info g)
;;                             :name 'state-info+g)
;; 
;; (define-packed-struct state-info+g (state-info g)
;;   )

;; (SB-KERNEL:%VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; (SB-KERNEL:%set-VECTOR-RAW-BITS (make-array 32 :initial-element 1 :element-type 'bit) 0)
;; sb-vm::single-float-bits
;; sb-kernel:single-float-bits
;; sb-ext:single-float-negative-infinity 

