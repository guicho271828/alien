
(define (domain d)
    (:requirements :strips :typing)
  (:types mytype)
  (:predicates (p ?x - mytype) (goal))
  (:action a
           :parameters (?x)
           :precondition (not (p ?x))
           :effect (goal)))

;; Is (not (p ?x)) equivalent to
;; 
;; (and (t ?x) (not (p ?x))) ?
;;
;; or is it equivalent to
;; 
;; (not (and (t ?x) (p ?x)))
;; ==
;; (or (not (t ?x)) (not (p ?x))) ?
