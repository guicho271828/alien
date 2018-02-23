(in-package :strips)

#+strips::phase/packed-structs
(strips.lib:define-packed-struct novelty ()
  (novelty 0 (runtime integer 0 *state-size*)))

#+strips::phase/full-compilation
(ftype* novelty-heuristics state+axioms (runtime integer 0 *state-size*))
#+strips::phase/full-compilation
(defun novelty-heuristics (state)

  )

#-(or strips::phase/packed-structs strips::phase/full-compilation)
(defun novelty ()
  (make-evaluator
   :storage '(novelty)
   :function 'novelty-heuristics))
