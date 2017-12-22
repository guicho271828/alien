
(define (problem p)
    (:domain d)
  (:objects o)
  (:init)
  (:goal (goal)))

;; If the solver treats (not (p ?x)) as (and (t ?x) (not (p ?x))),
;; then this problem is unsolvable because (t o) is not satisfied.

;; Fast downward solves this problem by applying (a o).
;; This means (not (p ?x)) == (or (not (t ?x)) (not (p ?x))).

;; In fact, it is not possible to achieve this.
;; (p ?x) implies (t ?x).
;; When (not (p ?x)), it does not imply (t ?x), so (t ?x) and (not (t ?x)) are both ok.
;; ???? still unclear, but FD should be handling this fine...
