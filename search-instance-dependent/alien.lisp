(in-package :strips)

(in-compilation-phase (phase/packed-structs)
  (strips.lib:define-packed-struct alien ()
    (alien 0 (runtime integer 0 *state-size*))))

(in-compilation-phase (phase/full-compilation)
  (ftype* alien-heuristics state+axioms (runtime integer 0 *state-size*))
  (defun alien-heuristics (state)

    ))

(in-compilation-phase ((not (or phase/packed-structs
                                phase/full-compilation)))
  (defun alien ()
    (make-evaluator
     :storage '(alien)
     :function 'alien-heuristics)))
