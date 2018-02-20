
(in-package :strips.lib)

;; I looked up various alternatives, but none satisfies my needs of storing and retrieving
;; certain data as compact as possible.

;; on SBCL:
;; Basic common lisp structure consumes additional 2 words for a single structure.
;; Also, slot types do not save the space usage, since they are fundamentally
;; arrays. Each slot consumes 1 word.
;; This multiplies the space usage when you make a huge number of small structures.

;; The code below implements a much more compact, C-like struct in a bit-vector.
;; The downside of this approach is that each bit-vector still consumes 2 words
;; for tagging. Do we need CFFI and alloc/free ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; VOP for pure SHL

(sb-c:defknown <<64 ((unsigned-byte 64) (mod 64)) (unsigned-byte 64)
    (sb-c::foldable
     sb-c::flushable
     sb-c::movable)
  :overwrite-fndb-silently t)

(in-package "SB-VM")


#+(or)
(define-vop (strips.lib::<<64)
  ;; Turned out, this code is wrong because it allows several arguments to be
  ;; assigned to the same register. See the failure example below.
  
  (:policy :fast-safe)
  (:translate strips.lib::<<64)
  (:args (int :scs (unsigned-reg) :target result)
         (shift :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-byte-64)
  (:generator 4
              (unless (location= result int)
                (move result int))
              (unless (location= ecx shift)
                (move ecx shift))
              (inst shl result :cl)))

;; 125:       90               NOP
;; 126:       90               NOP
;; 127:       90               NOP
;; 128:       488BD1           MOV RDX, RCX
;; 12B:       488BCA           MOV RCX, RDX
;; 12E:       90               NOP
;; 12F:       90               NOP
;; 130:       90               NOP
;; 131:       48D3E2           SHL RDX, CL
;; 134:       90               NOP
;; 135:       90               NOP
;; 136:       90               NOP

(define-vop (strips.lib::<<64)
  (:policy :fast-safe)
  (:translate strips.lib::<<64)
  (:args (number :scs (unsigned-reg) :target result
                 ;; I do not understand what this code actually means, even
                 ;; after I read the documentation of define-vop.
                 :load-if (not (and (sc-is number unsigned-stack)
                                    (sc-is result unsigned-stack)
                                    (location= number result))))
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
                    :load-if (not (and (sc-is number unsigned-stack)
                                       (sc-is result unsigned-stack)
                                       (location= number result)))))
  (:result-types unsigned-byte-64)
  (:generator 4
              (move result number)
              (move ecx amount)
              (inst shl result :cl)))

(in-package :strips.lib)

(defun <<64 (int shift)
  "Shift the integer like ASH, but discards the bits where ASH would cons to a bignum."
  (<<64 int shift))

#+(or)
(progn

(defun <<64-notype-dispatch-test (a)
  "12 is inlined"
  (<<64 a 12))

(defun <<64-folding-test ()
  "constant folded"
  (<<64 5 12))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "takes a packed type name or a type name, return the number of bits necessary to represent the value"
  (match type
    ((packed-struct-layout sizes)
     (reduce #'+ sizes))
    (_
     ;; packed-type
     (when (and (symbolp type)
                (packed-struct-layout-boundp type))
       (return-from size-of
         (size-of (symbol-packed-struct-layout type))))
     ;; common lisp type
     (restart-bind ((continue (lambda (c)
                                (log:warn "Substituting the size of ~a with 0" type)
                                (return-from size-of 0))))
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
          (* (size-of element-type) (reduce #'* (ensure-list dimensions)))))))))

;; constant fold
(define-compiler-macro size-of (&whole whole type &environment env)
  (if (constantp type env)
      (match type
        ((list 'quote type)
         (handler-case (size-of type)
           (error (c)
             (log:warn "In compiler macro expansion of ~a,~_ caught ~a :~_ ~a" whole (type-of c) c)
             whole)))
        (_
         whole))
      whole))

;; (size-of '(unsigned-byte 5))
;; (size-of '(integer 0 100))

(defun compute-offset (sizes)
  (iter (for s in sizes)
        (collecting sum)
        (summing s into sum)))

(defun ensure-packed-struct-layout (name-or-layout)
  (etypecase name-or-layout
    (symbol
     (symbol-packed-struct-layout name-or-layout))
    (packed-struct-layout
     name-or-layout)))

(defun merge-packed-struct-layout (structs &key (name (gensym)) names defaults types)
  (let ((result (reduce #'merge-packed-struct-layout-2
                        (mapcar #'ensure-packed-struct-layout structs)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; treating a big vector as a large integer and retrieve the value

(declaim (inline %packed-accessor-int))
(defun %packed-accessor-int (vector size position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           (simple-bit-vector vector))
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
           (simple-bit-vector vector))
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

#+(or)
(progn
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

(progn

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
)
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

;; testing optionals
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64 0)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64 1)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64 2)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 62 64 3)) ; 67

(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 65)) ; 66
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 66)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 64 67)) ; 67

(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 64)) ; 42
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 65)) ; 45
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 66)) ; 67
(defun %packed-accessor-test3 (vector) (declare (optimize (speed 3))) (%packed-accessor-int vector 63 67)) ; 67
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %packed-accessor-single-float))
(defun %packed-accessor-single-float (vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (simple-bit-vector vector))
  (sb-kernel:make-single-float (%packed-accessor-int vector 32 position)))

(declaim (inline %packed-accessor-double-float))
(defun %packed-accessor-double-float (vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (simple-bit-vector vector))
  (sb-kernel:make-double-float (%packed-accessor-int vector 32 (+ 32 position))
                               (%packed-accessor-int vector 32 position)))

(declaim (inline (setf %packed-accessor-single-float)))
(defun (setf %packed-accessor-single-float) (newval vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (single-float newval)
           (simple-bit-vector vector))
  (setf (%packed-accessor-int vector 32 position)
        (sb-kernel:single-float-bits newval)))

(declaim (inline (setf %packed-accessor-double-float)))
(defun (setf %packed-accessor-double-float) (newval vector position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (double-float newval)
           (simple-bit-vector vector))
  (setf (%packed-accessor-int vector 32 position)
        (sb-kernel:double-float-low-bits newval)
        (%packed-accessor-int vector 32 (+ 32 position))
        (sb-kernel:double-float-high-bits newval)))

#+(or)
(progn
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

(multiple-value-bind (signif exponent sign) (integer-decode-float -0.005)
  (print (list signif exponent sign))
  (print (scale-float
          (float
           (if (zerop sign)
               signif
               (- signif)))
          exponent)))
(* sign (scale-float signif exponent))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %packed-accessor-array))
(defun %packed-accessor-array (vector size position &optional (result (make-array size :element-type 'bit)))
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (fixnum size)
           (simple-bit-vector vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (declare (ignorable index-begin offset-begin))
    (multiple-value-bind (index-end offset-end) (floor (+ size position) 64)
      (declare (ignorable index-end offset-end))
      (labels ((rec (pos1 pos2)
                 (when (< pos2 (+ size position))
                   (setf (%packed-accessor-int result 64 pos1)
                         (%packed-accessor-int vector 64 pos2))
                   (rec (+ 64 pos1) (+ 64 pos2)))))
        ;; (declare (inline rec))
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
           (simple-bit-vector vector))
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
          ;; (declare (inline rec))
          (rec (- position offset-begin) 0
               (ash (%packed-accessor-int vector offset-begin (- position offset-begin))
                    ~offset-begin)))))))

#+(or)
(progn
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
  (declare ((simple-bit-vector 32) b))
  (setf (%packed-accessor-array b 5 12)  #*11111)
  0)

(defun packed-accessor-array-test2b (b)
  (declare ((simple-bit-vector 256) b))
  (setf (%packed-accessor-array b 5 16)  #*11111)
  (setf (%packed-accessor-array b 5 32)  #*11111)
  (setf (%packed-accessor-array b 5 128)  #*11111)
  b)

(defun packed-accessor-array-test3 (b)
  (declare ((simple-bit-vector 256) b))
  (setf (%packed-accessor-array b 256 0)
        #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
  0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define-packed-struct : store values in a bit-vector.

(lispn:define-namespace packed-struct-layout packed-struct-layout nil
                        "A namespace for the layout of the packed structure")

(defmacro define-packed-struct (name (&rest supertypes) &body slots)
  (let* ((slots (mapcar #'ensure-list slots))
         (layout (merge-packed-struct-layout
                  supertypes
                  :name name
                  :names (mapcar #'first slots)
                  :defaults (mapcar #'second slots)
                  :types (mapcar #'third slots)))
         (constructor (symbolicate 'make- name))
         (array-constructor (symbolicate 'make- name '-array)))
    (ematch layout
      ((packed-struct-layout names offsets sizes types)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (setf (symbol-packed-struct-layout ',name)
                ,layout)
          (declaim (inline ,constructor ,array-constructor)
                   (ftype (function (&key (:static *)) (simple-bit-vector ,(size-of layout)))
                          ,constructor)
                   (ftype (function (array-index &key (:static *))
                                    simple-bit-vector)
                          ,array-constructor))
          (defun ,constructor (&key static)
            (declare (ignore static))
            (error "slow runtime call to ~a!" ',constructor))
          (defun ,array-constructor (length &key static)
            (declare (ignore static))
            (error "slow runtime call to ~a!" ',array-constructor))
          (define-compiler-macro ,constructor (&key static)
            ;; note: sb-int:make-static-vector is also available
            (if static
                `(make-static-vector ,,(size-of layout) :element-type 'bit)
                `(make-array ,,(size-of layout) :element-type 'bit)))
          (define-compiler-macro ,array-constructor (length &key static)
            (if static
                `(make-static-vector (* ,length ,,(size-of layout)) :element-type 'bit)
                `(make-array (* ,length ,,(size-of layout)) :element-type 'bit)))
          ,@(mapcar (curry #'%packed-accessor-def name)
                    names offsets sizes types)
          ',name)))))

(defun %packed-accessor-def (struct-name slot-name offset size type)
  (let* ((type2 (handler-case
                    (introspect-environment:typexpand type)
                  (error () nil)))
         (array-result-argument nil)
         (writer
          (match type2
            ((type-r:integer-subtype)
             `(%packed-accessor-int instance ,size ,offset))
            ((type-r:single-float-type)
             `(%packed-accessor-single-float instance ,offset))
            ((type-r:double-float-type)
             `(%packed-accessor-double-float instance ,offset))
            ((type-r:array-subtype)
             (setf array-result-argument t)
             `(%packed-accessor-array instance ,size ,offset))
            ((type-r:member-type members)
             (assert (every #'integerp members))
             `(%packed-accessor-int instance ,size ,offset))
            (_
             `(error "~&in ~a: Unsupported type ~a~%" (sb-c:source-location) ',type2))))
         (reader
          ;; additional storage argument for avoiding consing, ala bit-and
          (if array-result-argument
              `(if result
                   (%packed-accessor-array instance ,size ,offset result)
                   (%packed-accessor-array instance ,size ,offset))
              ;; for non-array cases, reader = writer
              writer))
         (accessor-name
          (symbolicate struct-name '- slot-name)))
    `(progn
       (declaim (inline ,accessor-name (setf ,accessor-name)))
       (defun ,accessor-name (instance ,@(when array-result-argument '(&optional result)))
         (declare (simple-bit-vector instance)
                  ,@(when array-result-argument `(((or null (simple-bit-vector ,size)) result)))
                  (ignorable instance)
                  (optimize (speed 3) (safety 0)))
         ,reader)
       (defun (setf ,accessor-name) (newval instance)
         (declare (simple-bit-vector instance)
                  (ignorable instance newval)
                  (optimize (speed 3) (safety 0)))
         ,(if (eq 'error (first writer))
              writer
              `(setf ,writer newval))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ftype* packed-aref simple-bit-vector symbol fixnum &optional (or null simple-bit-vector) simple-bit-vector)
(defun packed-aref (array packed-type index &optional result)
  (declare (ignore array packed-type index result))
  (error "slow runtime call to packed-aref!"))

(define-compiler-macro packed-aref (array packed-type index &optional result &environment env)
  (assert (constantp packed-type env))
  (let ((size (eval `(size-of ,packed-type))))
    `(the (simple-bit-vector ,size)
          (let* ((result ,(or result
                              `(make-array ,size :element-type 'bit))))
            ,@(when result
                `((assert (= (length result) ,size))))
            (replace result ,array :start2 (* ,index ,size))))))

(ftype* (setf packed-aref) simple-bit-vector simple-bit-vector symbol fixnum simple-bit-vector)
(defun (setf packed-aref) (newval array packed-type index)
  (declare (ignore array packed-type index result))
  (error "slow runtime call to (setf packed-aref)!"))

(define-compiler-macro (setf packed-aref) (newval array packed-type index &environment env)
  (assert (constantp packed-type env))
  (let ((size (eval `(size-of ,packed-type))))
    `(replace ,array ,newval :start1 (* ,index ,size))))
