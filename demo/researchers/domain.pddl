 
; The researchers domain where a group of researchers meet at conferences, go to parties together after attending the conference to get to know other researches and eventually publish papers together.
; This is a very simple domain in which the main difficulty lies in grounding the problem due to the very high number of grounded actions

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

(:action beers-after-conference-m3-c2
                    :parameters (?m1 ?m2 ?m3 - mathematician ?c1 ?c2 - computer_scientist)
                    :precondition (and (not (= ?m1 ?m2)) (not (= ?m1 ?m3)) (not (= ?m2 ?m3)) (not (= ?c1 ?c2)) (knows ?m1 ?c1) (knows ?c1 ?m1) (knows ?m2 ?c2) (knows ?c2 ?m2) (knows ?m3 ?c2) (knows ?c2 ?m3))
                    :effect (and (knows ?c1 ?c2) (knows ?c1 ?m3) (knows ?c1 ?m2) (knows ?m1 ?c2) (knows ?m1 ?m3) (knows ?m1 ?m2) (knows ?c2 ?c1) (knows ?c2 ?m1) (knows ?m3 ?c1) (knows ?m3 ?m1) (knows ?m3 ?m2) (knows ?m2 ?c1) (knows ?m2 ?m1) (knows ?m2 ?m3)))
           
(:action beers-after-conference-m5-c2
                    :parameters (?m1 ?m2 ?m3 ?m4 ?m5 - mathematician ?c1 ?c2 - computer_scientist)
                    :precondition (and (not (= ?m1 ?m2)) (not (= ?m1 ?m3)) (not (= ?m1 ?m4)) (not (= ?m1 ?m5)) (not (= ?m2 ?m3)) (not (= ?m2 ?m4)) (not (= ?m2 ?m5)) (not (= ?m3 ?m4)) (not (= ?m3 ?m5)) (not (= ?m4 ?m5)) (not (= ?c1 ?c2)) (knows ?m1 ?m5) (knows ?c1 ?c2) (knows ?m4 ?c2) (knows ?m3 ?m1) (knows ?m1 ?m3) (knows ?c2 ?m4) (knows ?c2 ?c1) (knows ?m1 ?m2) (knows ?m2 ?m1) (knows ?m3 ?m2) (knows ?m5 ?m1) (knows ?m2 ?m3))
                    :effect (and (knows ?m4 ?c1) (knows ?m4 ?m1) (knows ?m4 ?m3) (knows ?m4 ?m5) (knows ?m4 ?m2) (knows ?c2 ?m1) (knows ?c2 ?m3) (knows ?c2 ?m5) (knows ?c2 ?m2) (knows ?c1 ?m4) (knows ?c1 ?m1) (knows ?c1 ?m3) (knows ?c1 ?m5) (knows ?c1 ?m2) (knows ?m1 ?m4) (knows ?m1 ?c2) (knows ?m1 ?c1) (knows ?m3 ?m4) (knows ?m3 ?c2) (knows ?m3 ?c1) (knows ?m3 ?m5) (knows ?m5 ?m4) (knows ?m5 ?c2) (knows ?m5 ?c1) (knows ?m5 ?m3) (knows ?m5 ?m2) (knows ?m2 ?m4) (knows ?m2 ?c2) (knows ?m2 ?c1) (knows ?m2 ?m5)))
           
(:action beers-after-conference-m2-c3
                    :parameters (?m1 ?m2 - mathematician ?c1 ?c2 ?c3 - computer_scientist)
                    :precondition (and (not (= ?m1 ?m2)) (not (= ?c1 ?c2)) (not (= ?c1 ?c3)) (not (= ?c2 ?c3)) (knows ?c1 ?m2) (knows ?m2 ?c1) (knows ?m1 ?c2) (knows ?c2 ?m1) (knows ?c1 ?c3) (knows ?m2 ?c3) (knows ?c3 ?c1) (knows ?c3 ?m2))
                    :effect (and (knows ?c2 ?c1) (knows ?c2 ?c3) (knows ?c2 ?m2) (knows ?m1 ?c1) (knows ?m1 ?c3) (knows ?m1 ?m2) (knows ?c1 ?c2) (knows ?c1 ?m1) (knows ?c3 ?c2) (knows ?c3 ?m1) (knows ?m2 ?c2) (knows ?m2 ?m1)))
           

       (:action publish-paper-2
       :parameters (?r1 - researcher ?r2 - researcher)
       :precondition (and (knows ?r1 ?r2) (knows ?r2 ?r1))
       :effect (and (coauthor ?r1 ?r2) (coauthor ?r2 ?r1))
       )
       

       (:action publish-paper-3
       :parameters (?r1 - researcher ?r2 - researcher ?r3 - researcher)
       :precondition (and (knows ?r1 ?r2) (knows ?r1 ?r3) (knows ?r2 ?r1) (knows ?r2 ?r3) (knows ?r3 ?r1) (knows ?r3 ?r2))
       :effect (and (coauthor ?r1 ?r2) (coauthor ?r1 ?r3) (coauthor ?r2 ?r1) (coauthor ?r2 ?r3) (coauthor ?r3 ?r1) (coauthor ?r3 ?r2))
       )
       

       (:action publish-paper-4
       :parameters (?r1 - researcher ?r2 - researcher ?r3 - researcher ?r4 - researcher)
       :precondition (and (knows ?r1 ?r2) (knows ?r1 ?r3) (knows ?r1 ?r4) (knows ?r2 ?r1) (knows ?r2 ?r3) (knows ?r2 ?r4) (knows ?r3 ?r1) (knows ?r3 ?r2) (knows ?r3 ?r4) (knows ?r4 ?r1) (knows ?r4 ?r2) (knows ?r4 ?r3))
       :effect (and (coauthor ?r1 ?r2) (coauthor ?r1 ?r3) (coauthor ?r1 ?r4) (coauthor ?r2 ?r1) (coauthor ?r2 ?r3) (coauthor ?r2 ?r4) (coauthor ?r3 ?r1) (coauthor ?r3 ?r2) (coauthor ?r3 ?r4) (coauthor ?r4 ?r1) (coauthor ?r4 ?r2) (coauthor ?r4 ?r3))
       )
       

       (:action publish-paper-5
       :parameters (?r1 - researcher ?r2 - researcher ?r3 - researcher ?r4 - researcher ?r5 - researcher)
       :precondition (and (knows ?r1 ?r2) (knows ?r1 ?r3) (knows ?r1 ?r4) (knows ?r1 ?r5) (knows ?r2 ?r1) (knows ?r2 ?r3) (knows ?r2 ?r4) (knows ?r2 ?r5) (knows ?r3 ?r1) (knows ?r3 ?r2) (knows ?r3 ?r4) (knows ?r3 ?r5) (knows ?r4 ?r1) (knows ?r4 ?r2) (knows ?r4 ?r3) (knows ?r4 ?r5) (knows ?r5 ?r1) (knows ?r5 ?r2) (knows ?r5 ?r3) (knows ?r5 ?r4))
       :effect (and (coauthor ?r1 ?r2) (coauthor ?r1 ?r3) (coauthor ?r1 ?r4) (coauthor ?r1 ?r5) (coauthor ?r2 ?r1) (coauthor ?r2 ?r3) (coauthor ?r2 ?r4) (coauthor ?r2 ?r5) (coauthor ?r3 ?r1) (coauthor ?r3 ?r2) (coauthor ?r3 ?r4) (coauthor ?r3 ?r5) (coauthor ?r4 ?r1) (coauthor ?r4 ?r2) (coauthor ?r4 ?r3) (coauthor ?r4 ?r5) (coauthor ?r5 ?r1) (coauthor ?r5 ?r2) (coauthor ?r5 ?r3) (coauthor ?r5 ?r4))
       )
       

       (:action meet-at-institution-2
       :parameters (?r1 - researcher ?r2 - researcher ?i - institution)
       :precondition (and  (not (= ?r1 ?r2)) (works_at ?r1 ?i) (works_at ?r2 ?i))
       :effect (and (knows ?r1 ?r2) (knows ?r2 ?r1))
       )
       

(:action read-math-paper
  :parameters (?r1 ?r2 - mathematician)
  :effect (knows ?r1 ?r2)
)

(:action read-cs-paper
  :parameters (?r1 ?r2 - computer_scientist)
  :effect (knows ?r1 ?r2)
)


)

