
(in-package :strips.lib)

(sb-c:defknown nop () null (sb-c::always-translatable)
  :overwrite-fndb-silently t)

(sb-c:defknown nop2 (*) null (sb-c::always-translatable)
  :overwrite-fndb-silently t)

(in-package "SB-VM")

(define-vop (strips.lib::nop)
  (:policy :fast-safe)
  (:translate strips.lib::nop)
  (:results (result :scs (descriptor-reg)))
  (:result-types list)
  (:generator 4
              (inst nop)))

(define-vop (strips.lib::nop2)
  (:policy :fast-safe)
  (:translate strips.lib::nop2)
  (:args (thing :scs (descriptor-reg)))
  (:arg-types t)
  (:results (result :scs (descriptor-reg)))
  (:result-types list)
  (:generator 4
              (inst nop)))




