
(in-package :cl-user)
(sb-c:defknown <<64 ((unsigned-byte 64) (mod 64)) (unsigned-byte 64)
    (sb-c::foldable
     sb-c::flushable
     sb-c::movable)
  :overwrite-fndb-silently t)

;; (sb-c:deftransform <<64 ((int shift) ((unsigned-byte 64) (integer 64 *)) (integer 0 0)) 0)

(in-package "SB-VM")

(define-vop (cl-user::<<64)
  (:policy :fast-safe)
  (:translate cl-user::<<64)
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

#+(or)
(define-vop (fast-ash-left/fixnum=>fixnum)
  (:translate ash)
  (:args (number :scs (any-reg) :target result
                 :load-if (not (and (sc-is number control-stack)
                                    (sc-is result control-stack)
                                    (location= number result))))
         (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (any-reg) :from (:argument 0)
                    :load-if (not (and (sc-is number control-stack)
                                       (sc-is result control-stack)
                                       (location= number result)))))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 3
    (move result number)
    (move ecx amount)
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)))

(print (info :function :info 'cl-user::<<64))

(in-package :cl-user)

(defun <<64 (int shift)
  "Shift the integer like ASH, but discards the bits where ASH would cons to a bignum."
  (format t "~b" (<<64 int shift))
  ;; (nth-value 0 (floor (<<64 int shift) 4))
  0
  )

;; ash
