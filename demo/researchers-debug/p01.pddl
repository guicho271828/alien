
(define (problem researchers-7-1) 
  (:domain researchers)
  (:objects researcher1 - computer_scientist
            researcher2 - mathematician
            researcher3 - mathematician
            researcher4 - computer_scientist
            institution1 - institution)

  (:init
   (works_at researcher1 institution1)
   (works_at researcher2 institution1)
   (works_at researcher3 institution1)
   (works_at researcher4 institution1)
   )

  (:goal 
   (and
    (coauthor researcher1 researcher4))))

