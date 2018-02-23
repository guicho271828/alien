(in-package :strips)

(in-compilation-phase (phase/packed-structs)
  (strips.lib:define-packed-struct novelty ()
    (novelty 0 (runtime integer 0 *state-size*))))

(in-compilation-phase (phase/full-compilation)
  (ftype* novelty-heuristics state+axioms (runtime integer 0 *state-size*))
  (defun novelty-heuristics (state)

    ))

(in-compilation-phase ((not (or phase/packed-structs
                                phase/full-compilation)))
  (defun novelty ()
    (make-evaluator
     :storage '(novelty)
     :function 'novelty-heuristics)))
