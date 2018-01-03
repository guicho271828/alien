(define (problem roverprob1234)
  (:domain Rover)
  (:objects
   rover0 - Rover
   waypoint0 waypoint1 waypoint2 waypoint3 - Waypoint)
  (:init (at rover0 waypoint3))

  (:goal (and (at rover0 waypoint0))))
