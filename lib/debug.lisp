
(in-package :alien.lib)

(sb-c:defknown nop () null (sb-c::always-translatable)
  :overwrite-fndb-silently t)

(sb-c:defknown nop2 (*) null (sb-c::always-translatable)
  :overwrite-fndb-silently t)

(in-package "SB-VM")

(define-vop (alien.lib::nop)
  (:policy :fast-safe)
  (:translate alien.lib::nop)
  (:results (result :scs (descriptor-reg)))
  (:result-types list)
  (:generator 4
              (inst nop)))

(define-vop (alien.lib::nop2)
  (:policy :fast-safe)
  (:translate alien.lib::nop2)
  (:args (thing :scs (descriptor-reg)))
  (:arg-types t)
  (:results (result :scs (descriptor-reg)))
  (:result-types list)
  (:generator 4
              (inst nop)))




