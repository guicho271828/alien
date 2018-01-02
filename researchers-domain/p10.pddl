 
(define (problem researchers-10-2) 
(:domain researchers)
(:objects researcher1 - computer_scientist
researcher2 - mathematician
researcher3 - computer_scientist
researcher4 - computer_scientist
researcher5 - computer_scientist
researcher6 - mathematician
researcher7 - mathematician
researcher8 - computer_scientist
researcher9 - mathematician
researcher10 - computer_scientist
institution1 - institution
institution2 - institution)

(:init
   (works_at researcher1 institution1)
(works_at researcher2 institution1)
(works_at researcher3 institution1)
(works_at researcher4 institution1)
(works_at researcher5 institution2)
(works_at researcher6 institution2)
(works_at researcher7 institution1)
(works_at researcher8 institution2)
(works_at researcher9 institution2)
(works_at researcher10 institution2)
)

(:goal 
(and
   (coauthor researcher1 researcher4)
(coauthor researcher3 researcher9)
(coauthor researcher6 researcher9)
(coauthor researcher7 researcher10)
(coauthor researcher2 researcher8)
(coauthor researcher2 researcher6)
(coauthor researcher8 researcher10)
(coauthor researcher2 researcher9)
(coauthor researcher4 researcher9)
(coauthor researcher1 researcher7)
(coauthor researcher3 researcher10)
(coauthor researcher2 researcher4)
(coauthor researcher3 researcher4)
(coauthor researcher4 researcher10)
(coauthor researcher3 researcher5)
(coauthor researcher1 researcher3)
(coauthor researcher6 researcher8)
(coauthor researcher1 researcher9)
(coauthor researcher6 researcher7)
(coauthor researcher4 researcher5)
(coauthor researcher5 researcher10)
(coauthor researcher5 researcher7)
)
)
)

