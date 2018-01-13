 
(define (problem researchers-7-1) 
(:domain researchers)
(:objects researcher1 - computer_scientist
researcher2 - mathematician
researcher3 - mathematician
researcher4 - computer_scientist
researcher5 - mathematician
researcher6 - computer_scientist
researcher7 - mathematician
institution1 - institution)

(:init
   (works_at researcher1 institution1)
(works_at researcher2 institution1)
(works_at researcher3 institution1)
(works_at researcher4 institution1)
(works_at researcher5 institution1)
(works_at researcher6 institution1)
(works_at researcher7 institution1)
)

(:goal 
(and
   (coauthor researcher5 researcher7)
(coauthor researcher4 researcher6)
(coauthor researcher1 researcher5)
(coauthor researcher1 researcher4)
(coauthor researcher3 researcher5)
(coauthor researcher2 researcher5)
(coauthor researcher3 researcher7)
(coauthor researcher4 researcher5)
(coauthor researcher6 researcher7)
(coauthor researcher2 researcher6)
)
)
)

