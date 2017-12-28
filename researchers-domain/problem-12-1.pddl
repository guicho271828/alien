 
(define (problem researchers-12-1) 
(:domain researchers)
(:objects researcher1 - mathematician
researcher2 - computer_scientist
researcher3 - computer_scientist
researcher4 - mathematician
researcher5 - computer_scientist
researcher6 - computer_scientist
researcher7 - mathematician
researcher8 - mathematician
researcher9 - mathematician
researcher10 - mathematician
researcher11 - mathematician
researcher12 - mathematician
institution1 - institution)

(:init
   (works_at researcher1 institution1)
(works_at researcher2 institution1)
(works_at researcher3 institution1)
(works_at researcher4 institution1)
(works_at researcher5 institution1)
(works_at researcher6 institution1)
(works_at researcher7 institution1)
(works_at researcher8 institution1)
(works_at researcher9 institution1)
(works_at researcher10 institution1)
(works_at researcher11 institution1)
(works_at researcher12 institution1)
)

(:goal 
(and
   (coauthor researcher3 researcher10)
(coauthor researcher2 researcher3)
(coauthor researcher2 researcher6)
(coauthor researcher6 researcher9)
(coauthor researcher5 researcher7)
(coauthor researcher4 researcher10)
(coauthor researcher9 researcher10)
(coauthor researcher5 researcher12)
(coauthor researcher8 researcher12)
(coauthor researcher5 researcher8)
(coauthor researcher2 researcher8)
(coauthor researcher9 researcher12)
(coauthor researcher1 researcher3)
(coauthor researcher2 researcher10)
(coauthor researcher3 researcher11)
(coauthor researcher1 researcher2)
(coauthor researcher8 researcher11)
(coauthor researcher9 researcher11)
(coauthor researcher5 researcher11)
(coauthor researcher7 researcher11)
(coauthor researcher10 researcher12)
(coauthor researcher3 researcher4)
(coauthor researcher2 researcher9)
(coauthor researcher1 researcher6)
(coauthor researcher3 researcher9)
(coauthor researcher1 researcher9)
(coauthor researcher1 researcher7)
(coauthor researcher7 researcher12)
(coauthor researcher6 researcher10)
(coauthor researcher3 researcher8)
(coauthor researcher1 researcher10)
(coauthor researcher1 researcher12)
(coauthor researcher4 researcher6)
)
)
)

