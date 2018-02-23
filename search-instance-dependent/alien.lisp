(in-package :strips)

#+strips::phase/packed-structs
(strips.lib:define-packed-struct alien ()
  (alien 0 (runtime integer 0 *state-size*)))

#+strips::phase/full-compilation
(ftype* alien-heuristics state+axioms (runtime integer 0 *state-size*))
#+strips::phase/full-compilation
(defun alien-heuristics (state)

  )

#-(or strips::phase/packed-structs strips::phase/full-compilation)
(defun alien ()
  (make-evaluator
   :storage '(alien)
   :function 'alien-heuristics))
