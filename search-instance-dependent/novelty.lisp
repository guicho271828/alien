(in-package :strips)

#|


|#

(in-compilation-phase ((and novelty phase/packed-structs))
  (strips.lib:define-packed-struct novelty ()
    (novelty 0 (runtime integer 0 *state-size*))))

(in-compilation-phase ((and novelty phase/full-compilation))
  (ftype* novelty-heuristics state+axioms (runtime integer 0 *state-size*))
  (defun novelty-heuristics (state) 0))

(in-compilation-phase ((not (or phase/packed-structs
                                phase/full-compilation)))
  (defun novelty ()
    (push 'novelty *optional-features*)
    (ensure-relaxed-sg)
    (make-evaluator
     :storage '(novelty)
     :function 'novelty-heuristics)))
