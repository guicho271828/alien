
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

;; Optimizing operations including BYTESPEC is impossible!

(defun sbcl-failure (newval oldval offset size)
  ;; SB-KERNEL:%DPB
  ;; SB-C::%DEPOSIT-FIELD-DERIVE-TYPE-AUX
  ;; note: unable to
  ;;   convert to inline logical operations
  ;; due to type uncertainty:
  ;;   The result is a (VALUES (UNSIGNED-BYTE 126) &OPTIONAL), not a (VALUES (UNSIGNED-BYTE 64) &REST T).
  (declare ((mod 64) offset size)
           ((unsigned-byte 64) newval oldval)
           (optimize (speed 3)))
  (assert (< (+ offset size) 64))
  (dpb newval (byte size offset) oldval))

(defun sbcl-failure2 (newval oldval offset size)
  (declare ((mod 64) offset size)
           ((unsigned-byte 64) newval oldval)
           (optimize (speed 3)))
  (assert (< (+ offset size) 64))
  (let ((size2 (+ offset size)))
    (declare ((mod 64) size2))
    (let ((size3 (- size2 offset)))
      (dpb newval (byte size3 offset) oldval))))

(defun sbcl-failure3 (newval oldval offset end)
  (declare ((mod 64) offset end)
           ((unsigned-byte 64) newval oldval)
           (optimize (speed 3)))
  (let ((size (- end offset)))
    (dpb newval (byte size offset) oldval)))

