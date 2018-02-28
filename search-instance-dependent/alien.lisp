(in-package :strips)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

(in-compilation-phase ((and alien phase/packed-structs))
  (strips.lib:define-packed-struct alien ()
    (alien 0 (runtime integer 0 *state-size*))))

(in-compilation-phase ((and alien phase/full-compilation))
  (ftype* alien-heuristics state+axioms (runtime integer 0 *state-size*))
  (defun alien-heuristics (state) 0))

(in-compilation-phase ((not (or phase/packed-structs
                                phase/full-compilation)))
  (defun alien ()
    (push 'alien *optional-features*)
    (ensure-delete-relaxed-sg)
    (make-evaluator
     :storage '(alien)
     :function 'alien-heuristics)))
