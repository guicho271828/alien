(in-package :strips)

#+strips::phase/packed-structs
(strips.lib:define-packed-struct ff ()
  (ff 0 (runtime integer 0 *state-size*)))

#+strips::phase/full-compilation
(ftype* ff-heuristics state+axioms (runtime integer 0 *state-size*))
#+strips::phase/full-compilation
(defun ff-heuristics (state)

  )

#-(or strips::phase/packed-structs strips::phase/full-compilation)
(defun ff ()
  (make-evaluator
   :storage '(ff)
   :function 'ff-heuristics))
