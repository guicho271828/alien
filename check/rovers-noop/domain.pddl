(define (domain Rover)
  (:requirements :typing)
  (:types rover waypoint)

  (:predicates (at ?x - rover ?y - waypoint))

  
  (:action navigate
           :parameters (?x - rover ?y - waypoint ?z - waypoint) 
           :precondition (and (at ?x ?y))
           :effect (and (not (at ?x ?y)) (at ?x ?z))))
