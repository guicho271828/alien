
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
#+(or)
(progn
(sb-c:defknown <<64 ((unsigned-byte 64) (mod 64)) (unsigned-byte 64)
    (sb-c::foldable
     sb-c::flushable
     sb-c::movable)
  :overwrite-fndb-silently t)

(sb-c:deftransform <<64 ((int shift) (* (eql 0)) *)
  'int)

;; (print (sb-int:info :function :info '<<64))

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
)

(progn
(ftype* <<64 (unsigned-byte 64) (mod 64) (unsigned-byte 64))
(declaim (inline <<64))
(defun <<64 (int shift)
  "Shift the integer like ASH, but discards the bits where ASH would cons to a bignum."
  (mask-field (byte 64 0) (ash int shift)))
)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *packer-debug* nil))

(defmacro print-when-debug (&rest args)
  (when *packer-debug*
    `(print ,@args)))

(declaim (inline %packed-accessor-int))
(defun %packed-accessor-int (vector size position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           (simple-bit-vector vector))
  (assert (<= (+ size position) (length vector)) nil
          "in %packed-accessor-int: (<= (+ size position)=~a (length vector)=~a)"
          (+ size position) (length vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (let (;; (size (max 0 (min size (- (length vector) position))))
          (remaining (- 64 offset-begin)))
      ;; offset-begin: 0-63
      ;; remaining:    1-64
      (cond
        ((<= size remaining)    ;when position=63, remaining = 1, size=1
         (print-when-debug :read-int-one-word)
         ;; -----11- -> 11------
         (mask-field (byte size 0)
                     (ash (sb-kernel:%vector-raw-bits vector index-begin)
                          (- offset-begin))))
        (t
         (print-when-debug :read-int-two-words)
         ;; -----111 222----- -> 111222--
         (logior (ash (sb-kernel:%vector-raw-bits vector index-begin)
                      (- offset-begin))
                 (ash (<<64 (sb-kernel:%vector-raw-bits vector (1+ index-begin))
                            (- 128 offset-begin size))
                      (- size 64))))))))

(declaim (inline %packed-accessor-int-unsafe))
(defun %packed-accessor-int-unsafe (vector size position)
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure.
WARNING: this version does not mask the bits outside SIZE."
  (declare (fixnum position)
           ((integer 0 64) size)
           (simple-bit-vector vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (let ((remaining (- 64 offset-begin)))
      ;; offset-begin: 0-63
      ;; remaining:    1-64
      (cond
        ((<= size remaining)    ;when position=63, remaining = 1, size=1
         (print-when-debug :read-int-one-word)
         ;; -----11- -> 11------
         (ash (sb-kernel:%vector-raw-bits vector index-begin)
              (- offset-begin)))
        (t
         (print-when-debug :read-int-two-words)
         ;; -----111 222----- -> 111222--
         (logior (ash (sb-kernel:%vector-raw-bits vector index-begin)
                      (- offset-begin))
                 (ash (<<64 (sb-kernel:%vector-raw-bits vector (1+ index-begin))
                            (- 128 offset-begin size))
                      (- size 64))))))))

(declaim (inline (setf %packed-accessor-int)))
(defun (setf %packed-accessor-int) (newval vector size position)
  "position: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           ((unsigned-byte 64) newval)
           (simple-bit-vector vector))
  (assert (<= (+ size position) (length vector)) nil
          "in (setf %packed-accessor-int): (<= (+ size position)=~a (length vector)=~a)"
          (+ size position) (length vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (let ((remaining (- 64 offset-begin)))
      (cond
        ((<= size remaining)
         (print-when-debug :write-int-one-word)
         ;; following SETF code expands into dpb, where SBCL does not know
         ;; (size + offset-begin) is below 64
         #+(or)
         (setf (ldb (byte size offset-begin)
                    (sb-kernel:%vector-raw-bits vector index-begin))
               newval)

         (setf (sb-kernel:%vector-raw-bits vector index-begin)
               (let ((int (sb-kernel:%vector-raw-bits vector index-begin)))
                 ;; from deftransform of SB-KERNEL:%DPB
                 #+(or)
                 (let ((mask (ldb (byte size 0) -1)))
                   (logior (ash (logand new mask) posn)
                           (logand int (lognot (ash mask posn)))))

                 ;; using <<64 instead
                 (let ((mask (ldb (byte size 0) -1)))
                   (logior (<<64 (logand newval mask) offset-begin)
                           (logand int (lognot (ash mask offset-begin))))))))
        (t
         (print-when-debug :write-int-two-words1)
         #+(or)
         (setf (ldb (byte remaining offset-begin)
                    (sb-kernel:%vector-raw-bits vector index-begin))
               (ldb (byte remaining 0)
                    newval))
         (setf (sb-kernel:%vector-raw-bits vector index-begin)
               (let ((int (sb-kernel:%vector-raw-bits vector index-begin))
                     (newval (ldb (byte remaining 0) newval)))
                 (let ((mask (ldb (byte remaining 0) -1)))
                   (logior (<<64 (logand newval mask) offset-begin)
                           (logand int (lognot (ash mask offset-begin)))))))
         (print-when-debug :write-int-two-words2)
         (setf (ldb (byte (- size remaining) 0)
                    (sb-kernel:%vector-raw-bits vector (1+ index-begin)))
               (ldb (byte (- size remaining) remaining)
                    newval)))))))

(declaim (inline (setf %packed-accessor-int-unsafe)))
(defun (setf %packed-accessor-int-unsafe) (newval vector size position)
  "position: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           ((integer 0 64) size)
           ((unsigned-byte 64) newval)
           (simple-bit-vector vector))
  (multiple-value-bind (index-begin offset-begin) (floor position 64)
    (let ((remaining (- 64 offset-begin)))
      (cond
        ((<= size remaining)
         (print-when-debug :write-int-one-word)
         (setf (sb-kernel:%vector-raw-bits vector index-begin)
               (let ((int (sb-kernel:%vector-raw-bits vector index-begin)))
                 (let ((mask (ldb (byte size 0) -1)))
                   (logior (<<64 (logand newval mask) offset-begin)
                           (logand int (lognot (ash mask offset-begin))))))))
        (t
         (print-when-debug :write-int-two-words1)
         (setf (sb-kernel:%vector-raw-bits vector index-begin)
               (let ((int (sb-kernel:%vector-raw-bits vector index-begin))
                     (newval (ldb (byte remaining 0) newval)))
                 (let ((mask (ldb (byte remaining 0) -1)))
                   (logior (<<64 (logand newval mask) offset-begin)
                           (logand int (lognot (ash mask offset-begin)))))))
         (print-when-debug :write-int-two-words2)
         (setf (ldb (byte (- size remaining) 0)
                    (sb-kernel:%vector-raw-bits vector (1+ index-begin)))
               (ldb (byte (- size remaining) remaining)
                    newval)))))))

#+(or)
(progn

(defun %vector-raw-bits-test ()
  (let ((b #*0000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000001100000000000000000000000000000000000000000000000000000000000000))
    (assert (= 0 (SB-KERNEL:%VECTOR-RAW-BITS b 0)))
    (assert (= 1 (SB-KERNEL:%VECTOR-RAW-BITS b 1)))
    (assert (= 2 (SB-KERNEL:%VECTOR-RAW-BITS b 2)))
    (assert (= 3 (SB-KERNEL:%VECTOR-RAW-BITS b 3)))))

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

(defun %packed-accessor-store-load-test ()
  (let ((b (make-array 5 :element-type 'bit :initial-element 1)))
    (assert (= 31 (%packed-accessor-int b 5 0)))
    ;; let's go strict. below all signals a runtime error
    ;; (assert (= 31 (%packed-accessor-int b 32 0))) ; out-of-bound bits are treated as zero ?
    ;; (assert (= 31 (%packed-accessor-int b 64 0)))
    ;; ;; (assert (= 31 (%packed-accessor-int b 1024 0))) ; should not compile, 1024 > 64
    ;; (assert (= 15 (%packed-accessor-int b 64 1)))
    ;; (assert (= 7 (%packed-accessor-int b 64 2)))
    ;; (assert (= 3 (%packed-accessor-int b 64 3)))
    ;; (assert (= 1 (%packed-accessor-int b 64 4)))
    ;; (assert (= 0 (%packed-accessor-int b 64 5)))
    ;; (assert (= 0 (%packed-accessor-int b 5 1024)))
    )

  ;; should fail to compile
  ;; (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
  ;;   (setf (%packed-accessor-int b 64 0) (expt 2 64)))
  
  (let ((b (make-array 5 :element-type 'bit :initial-element 0)))
    ;; setting out-of-bounds bit do not take effect
    (setf (%packed-accessor-int b 5 0) 31)
    (assert (= 31 (%packed-accessor-int b 5 0)))
    (fill b 0)

    ;; should signal a runtime error
    ;; (setf (%packed-accessor-int b 5 1) 31)
    )
  
  (let ((b (make-array 64 :element-type 'bit :initial-element 0)))
    (setf (%packed-accessor-int b 4 0) #b1111)
    (assert (equal b #*1111000000000000000000000000000000000000000000000000000000000000))
    (assert (= #b1111 (%packed-accessor-int b 4 0)))
    (setf (%packed-accessor-int b 4 4) #b11000) ; the fifth digit (leftmost 1) is out of bounds
    (assert (equal b #*1111000100000000000000000000000000000000000000000000000000000000))
    (assert (= #b1000 (%packed-accessor-int b 4 4))))
  
  (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
    (setf (%packed-accessor-int b 2 63) #b11) ; across word boundary
    (assert (= #b1 (%packed-accessor-int b 1 63)))
    (assert (= #b1 (%packed-accessor-int b 1 64)))
    (assert (= #b11 (%packed-accessor-int b 2 63)))
    (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000000000000)))
  (print :ok!))
(%packed-accessor-store-load-test)

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
(defun %packed-accessor-array (vector size position &optional (newval (make-array size :element-type 'bit)))
  "offset: number of bits from the beginning of the structure
size: number of bits for the structure"
  (declare (fixnum position)
           (fixnum size)
           (simple-bit-vector vector))
  (assert (<= (+ size position) (length vector)) nil
          "in %packed-accessor-array: (<= (+ size position)=~a (length vector)=~a)"
          (+ size position) (length vector))
  (assert (= size (length newval)) nil
          "in %packed-accessor-array: (= size=~a (length newval)=~a)"
          size (length newval))
  (labels ((rec (vec-pos new-pos)
             (cond
               ((<= size (+ new-pos 64))
                (setf (%packed-accessor-int newval (- size new-pos) new-pos)
                      (%packed-accessor-int vector (- size new-pos) vec-pos)))
               (t
                (setf (%packed-accessor-int newval 64 new-pos)
                      (%packed-accessor-int vector 64 vec-pos))
                (rec (+ 64 vec-pos) (+ 64 new-pos))))))
    (declare (inline rec))
    (rec position 0)
    newval))

(declaim (inline (setf %packed-accessor-array)))
(defun (setf %packed-accessor-array) (newval vector size position)
  "position: number of bits from the beginning of the structure
size: number of bits for the structure.
If NEWVAL length is larger than the size, then the remaining portion of the vector is discarded."
  (declare (fixnum position size)
           (simple-array newval)
           (simple-bit-vector vector))
  (assert (<= (+ size position) (length vector)) nil
          "in (setf %packed-accessor-array): (<= (+ size position)=~a (length vector)=~a)"
          (+ size position) (length vector))
  (assert (= size (length newval)) nil
          "in (setf %packed-accessor-array): (= size=~a (length newval)=~a)"
          size (length newval))

  (labels ((rec (vec-pos new-pos)
             (cond
               ((<= size (+ new-pos 64))
                (setf (%packed-accessor-int vector (- size new-pos) vec-pos)
                      (%packed-accessor-int newval (- size new-pos) new-pos)))
               (t
                (setf (%packed-accessor-int vector 64 vec-pos)
                      (%packed-accessor-int newval 64 new-pos))
                (rec (+ 64 vec-pos) (+ 64 new-pos))))))
    (declare (inline rec))
    (rec position 0)
    newval))

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
          #*1010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000001010101010101010101010101010101010101010101010101010101010101010)))
  (let ((b (make-array 256 :initial-element 0 :element-type 'bit)))
    (setf (%packed-accessor-array b 256 0)
          #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)))
  (print :ok!))

(packed-accessor-array-test1)

(defun packed-accessor-array-test2 ()
  
  (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
    (setf (%packed-accessor-array b 5 16) #*11111)
    (setf (%packed-accessor-array b 5 32) #*11111)
    (setf (%packed-accessor-array b 5 64) #*11111)
    (assert (equal b #*00000000000000001111100000000000111110000000000000000000000000001111100000000000000000000000000000000000000000000000000000000000)))
  (print :ok!)
  
  (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
    (setf (%packed-accessor-array b 64 64)
          #*1010101010101010101010101010101010101010101010101010101010101010)
    (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000001010101010101010101010101010101010101010101010101010101010101010)))
  (print :ok!))

(packed-accessor-array-test2)

(defun packed-accessor-array-test3 ()
  (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
    (setf (%packed-accessor-array b 5 62) #*11111)
    (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000111110000000000000000000000000000000000000000000000000000000000000))
    (print b))
  ;; (let ((b (make-array 128 :element-type 'bit :initial-element 0)))
  ;;   (setf (%packed-accessor-array b 5 62) (make-array 5 :element-type 'bit :initial-element 1))
  ;;   ;; (assert (equal b #*00000000000000000000000000000000000000000000000000000000000000111110000000000000000000000000000000000000000000000000000000000000))
  ;;   (print b))
  (print :ok!))

(packed-accessor-array-test3)

(defun packed-accessor-array-setter-disassembly (b c)
  (declare ((simple-bit-vector 512) b)
           ((simple-bit-vector 5) c)
           (optimize (speed 3)))
  (setf (%packed-accessor-array b 5 12) c)
  (print :ok!)
  0)

(packed-accessor-array-setter-disassembly (make-array 512 :element-type 'bit :initial-element 0)
                                          (make-array 5 :element-type 'bit :initial-element 1))

(defun packed-accessor-array-setter-disassembly2 (b)
  (declare ((simple-bit-vector 128) b)
           (optimize (speed 3)))
  (setf (%packed-accessor-array b 5 12) #*11111)
  (print :ok!)
  0)

(packed-accessor-array-setter-disassembly2 (make-array 512 :element-type 'bit :initial-element 0))

(defun packed-accessor-array-loader-disassembly (b)
  (declare ((simple-bit-vector 512) b)
           (optimize (speed 3)))
  ;; should compile to 4 unrolled inlined accesses
  (print (%packed-accessor-array b 256 64))
  (print :ok!)
  0)

(packed-accessor-array-loader-disassembly (make-array 512 :element-type 'bit :initial-element 0))

(defun packed-accessor-array-loader-disassembly2 (b)
  (declare ((simple-bit-vector 512) b)
           (optimize (speed 3)))
  ;; should compile to 4 unrolled inlined accesses
  (print (%packed-accessor-array b 255 65))
  (print :ok!)
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
