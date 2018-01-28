
;; The researchers domain where a group of researchers meet at conferences, go to
;; parties together after attending the conference to get to know other
;; researches and eventually publish papers together.  This is a very simple
;; domain in which the main difficulty lies in grounding the problem due to the
;; very high number of grounded actions

(define (domain researchers)
  (:requirements :strips :typing :equality)
  (:types researcher institution - object 
          mathematician computer_scientist - researcher
          )

  (:predicates
   (coauthor ?x - researcher ?y - researcher)
   (knows ?x - researcher ?y - researcher)
   (works_at ?x - researcher ?y - institution)
   )

  (:action beers-after-conference-m2-c2
           :parameters (?m1 ?m2 - mathematician ?c1 ?c2 - computer_scientist)
           :precondition (and (not (= ?m1 ?m2))
                              (not (= ?c1 ?c2))
                              (knows ?m1 ?c1)
                              (knows ?c1 ?m1)
                              (knows ?m2 ?c2)
                              (knows ?c2 ?m2))
           :effect (and (knows ?c1 ?c2)
                        (knows ?c1 ?m2)
                        (knows ?m1 ?c2)
                        (knows ?m1 ?m2)
                        (knows ?c2 ?c1)
                        (knows ?c2 ?m1)
                        (knows ?m2 ?c1)
                        (knows ?m2 ?m1)))

  (:action publish-paper-2
           :parameters (?r1 - researcher ?r2 - researcher)
           :precondition (and (knows ?r1 ?r2)
                              (knows ?r2 ?r1))
           :effect (and (coauthor ?r1 ?r2)
                        (coauthor ?r2 ?r1)))

  (:action meet-at-institution-2
           :parameters (?r1 - researcher ?r2 - researcher ?i - institution)
           :precondition (and (not (= ?r1 ?r2))
                              (works_at ?r1 ?i)
                              (works_at ?r2 ?i))
           :effect (and (knows ?r1 ?r2) (knows ?r2 ?r1)))
  

  (:action read-math-paper
           :parameters (?r1 ?r2 - mathematician)
           :effect (knows ?r1 ?r2))

  (:action read-cs-paper
           :parameters (?r1 ?r2 - computer_scientist)
           :effect (knows ?r1 ?r2)))

