
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
        ((or (= index-begin index-end)
             (= 0 offset-end))
         (ldb (byte size offset-begin)
              (sb-kernel:%vector-raw-bits vector index-begin)))
        (t
         (the (unsigned-byte 64)
              (+ (ldb (byte (- 64 offset-begin) offset-begin)
                      (sb-kernel:%vector-raw-bits vector index-begin))
                 (ash (ldb (byte offset-end 0)
                           (sb-kernel:%vector-raw-bits vector index-end))
                      (- 64 offset-begin)))))))))

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
        ((or (= index-begin index-end)
             (= 0 offset-end))
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

;; checking the disassembly

(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 62 64))) ; 42
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 62 65))) ; 45
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 62 66))) ; 39
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 62 67))) ; 67

(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 64 64))) ; 42
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 64 65))) ; 66
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 64 66))) ; 67
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 64 67))) ; 67

(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 63 64))) ; 42
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 63 65))) ; 45
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 63 66))) ; 67
(defun %packed-accessor-test1 (vector) (declare (optimize (speed 3))) (ldb (byte 62 0) (%packed-accessor-int vector 63 67))) ; 67

;; checking the store/load

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

;; checking type propagation

(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 65)) ; 45
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 66)) ; 39
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 67)) ; 67

(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 65)) ; 66
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 66)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 67)) ; 67

(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 65)) ; 45
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 66)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 67)) ; 67

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %packed-accessor-single-float))
(defun %packed-accessor-single-float (vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (simple-array vector))
  (sb-kernel:make-single-float (%packed-accessor-int vector 32 position)))

(declaim (inline %packed-accessor-double-float))
(defun %packed-accessor-double-float (vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (simple-array vector))
  (sb-kernel:make-double-float (%packed-accessor-int vector 32 (+ 32 position))
                               (%packed-accessor-int vector 32 position)))

(declaim (inline (setf %packed-accessor-single-float)))
(defun (setf %packed-accessor-single-float) (newval vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (single-float newval)
           (simple-array vector))
  (setf (%packed-accessor-int vector 32 position)
        (sb-kernel:single-float-bits newval)))

(declaim (inline (setf %packed-accessor-double-float)))
(defun (setf %packed-accessor-double-float) (newval vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (double-float newval)
           (simple-array vector))
  (setf (%packed-accessor-int vector 32 position)
        (sb-kernel:double-float-low-bits newval)
        (%packed-accessor-int vector 32 (+ 32 position))
        (sb-kernel:double-float-high-bits newval)))

(defun %packed-accessor-float-test ()
  ;; length 64
  (let ((b #*0000000000000000000000000000000000000000000000000000000000000000))
    (setf (%packed-accessor-single-float b 0) 3.14)
    (setf (%packed-accessor-single-float b 32) 2.71)
    (print (%packed-accessor-single-float b 0))
    (print (%packed-accessor-single-float b 32))
    (print b))
  
  ;; length 128
  (let ((b #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
    #+(or)
    (setf (%packed-accessor-double-float b 0) 3.14) ; compilation error
    (setf (%packed-accessor-double-float b 0) 3.14d0)
    (setf (%packed-accessor-double-float b 64) 2.71d0)
    (print (%packed-accessor-double-float b 0))
    (print (%packed-accessor-double-float b 64))
    (print b)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %packed-accessor-array))
(defun %packed-accessor-array (vector size position &optional (result (make-array size :element-type 'bit)))
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (fixnum size)
           (simple-array vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (declare (ignorable index-begin offset-begin))
    (multiple-value-bind (index-end offset-end) (floor (+ size position) 64)
      (declare (ignorable index-end offset-end))
      (labels ((rec (pos1 pos2)
                 (when (< pos2 (+ size position))
                   (setf (%packed-accessor-int result 64 pos1)
                         (%packed-accessor-int vector 64 pos2))
                   (rec (+ 64 pos1) (+ 64 pos2)))))
        (declare (inline rec))
        (setf (%packed-accessor-int result (- 64 offset-begin) 0)
              (%packed-accessor-int vector (- 64 offset-begin) position))
        (rec (- 64 offset-begin) (+ (- 64 offset-begin) position))
        result))))

(declaim (inline (setf %packed-accessor-array)))
(defun (setf %packed-accessor-array) (newval vector size position)
  "position: number of bits from the beginning of the structure
size: number of bits for the structure.
If NEWVAL length is larger than the size, then the remaining portion of the vector is not copied."
  (declare (fixnum position)
           (fixnum size)
           (simple-array newval)
           (simple-array vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (declare (ignorable index-begin offset-begin))
    (multiple-value-bind (index-end offset-end) (floor (+ size position) 64)
      (declare (ignorable index-end offset-end))
      ;; 0   45 7 8
      ;;      *    position
      ;; <--->     offset-begin
      ;;      <->  ~offset-begin
      ;;                           *     size+position
      ;; 00000111   22222 222  33330000  vector
      ;;     / /    
      ;;    <->                     copied before rec
      ;;   / /      <------->       copied in the first rec (full word copy)
      ;;  / /                  <--> copied in the second rec
      ;; 111 22222  222 3333 0   newval
      ;;            *            vec-pos (first iteration)
      ;; <------->               prev
      ;;            <------->    now
      ;;                       * vec-pos (second iteration)
      (let ((~offset-begin (- 64 offset-begin)))
        ;; (print (list :offset-begin offset-begin
        ;;              :~offset-begin ~offset-begin
        ;;              :offset-end offset-end
        ;;              :position position))
        ;; (print vector)
        (labels ((rec (vec-pos new-pos prev)
                   ;; (print (list :vec-pos vec-pos
                   ;;              :new-pos new-pos
                   ;;              :prev    prev))
                   ;; (print (list `(< vec-pos (+ size position) (+ 64 vec-pos))
                   ;;              `(< ,vec-pos ,(+ size position) ,(+ 64 vec-pos))))
                   (let ((now (%packed-accessor-int newval 64 new-pos)))
                     (if (<= (+ 64 vec-pos) (+ size position))
                         (progn
                           ;; (format t "~&~64,'0b~%" prev)
                           ;; (format t "~&~64,'0b~%" now)
                           ;; (format t "~&~64,'0b~%" (%packed-accessor-int vector 64 vec-pos))
                           ;; (format t "~&~64,'0b"
                           ;;         (+ (ldb (byte offset-begin ~offset-begin) prev)
                           ;;            (ash (ldb (byte ~offset-begin 0) now) offset-begin)))
                           ;; (print `(+ (ldb (byte ,offset-begin ,~offset-begin) prev)
                           ;;            (ash (ldb (byte ,~offset-begin 0) now) ,offset-begin)))
                           (setf (%packed-accessor-int vector 64 vec-pos)
                                 (+ (ldb (byte offset-begin ~offset-begin) prev)
                                    (ash (ldb (byte ~offset-begin 0) now) offset-begin)))
                           (rec (+ 64 vec-pos) (+ 64 new-pos) now))
                         ;; vec-pos < size+position < vec-pos+64
                         (setf (%packed-accessor-int vector offset-end vec-pos)
                               (+ (ldb (byte offset-end ~offset-begin) prev)
                                  (ash (ldb (byte ~offset-begin 0) now) offset-begin)))))))
          (declare (inline rec))
          (rec (- position offset-begin) 0
               (ash (%packed-accessor-int vector offset-begin (- position offset-begin))
                    ~offset-begin)))))))

(defun packed-accessor-array-test1 ()
  (let ((b (make-array 32 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 5 12)  #*11111)
    (assert (equal b #*00000000000011111000000000000000)))
  (let ((b (make-array 64 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 64 0)
          #*1100110011001100110011001100110011001100110011001100110011001100)
    (assert (equal b #*1100110011001100110011001100110011001100110011001100110011001100)))
  (let ((b (make-array 128 :initial-element 0 :element-type 'bit)))
    (iter (for i from 0 to 128 by 10)
          (setf (%packed-accessor-array b 5 i) #*11111))
    (assert (equal b #*11111000001111100000111110000011111000001111100000111110000011111000001111100000111110000011111000001111100000111110000011111000)))
  (let ((b (make-array 128 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 128 0)
          #*10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)))
  (let ((b (make-array 128 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 64 64)
          #*10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000001010101010101010101010101010101010101010101010101010101010101010)))
  (let ((b (make-array 256 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 256 0)
          #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)))
  0)

(defun packed-accessor-array-test2 (b)
  (declare ((simple-array 32) b))
  (setf (%packed-accessor-array b 5 12)  #*11111)
  0)

(defun packed-accessor-array-test2b (b)
  (declare ((simple-array 256) b))
  (setf (%packed-accessor-array b 5 16)  #*11111)
  (setf (%packed-accessor-array b 5 32)  #*11111)
  (setf (%packed-accessor-array b 5 128)  #*11111)
  b)

(defun packed-accessor-array-test3 (b)
  (declare ((simple-array 256) b))
  (setf (%packed-accessor-array b 256 0)
        #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; sb-kernel:make-single-float
;; sb-kernel:make-double-float
