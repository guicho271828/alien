
(define (domain d)
  (:requirements :strips :typing)
  (:predicates (p ?x) (goal))
  (:action A
           :parameters (?x)
           :precondition (not (p ?x))
           :effect (goal)))

;; when negative preconditions are ignored,
;; action A is always reachable.

;; otherwise, it is reachable only when (p ?x) is initially false and never added, or reachable but can be deleted.
;; since no action adds or deletes (p ?x), 
