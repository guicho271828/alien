(in-package :strips)

(in-compilation-phase (phase/packed-structs)
  (strips.lib:define-packed-struct ff ()
    (ff 0 (runtime integer 0 *state-size*))))

(in-compilation-phase (phase/full-compilation)
  (ftype* ff-heuristics state+axioms (runtime integer 0 *state-size*))
  (defun ff-heuristics (state)

    ))

(in-compilation-phase ((not (or phase/packed-structs
                                phase/full-compilation)))
  (defun ff ()
    (make-evaluator
     :storage '(ff)
     :function 'ff-heuristics)))
