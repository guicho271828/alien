(in-package :alien)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

#|

delete relaxed task is solvable when the task is solvable.

IW(1) is complete on delete-relaxed task.
It finishes in polynomial time of O(facts).

IW(1) + op-once constraint is also complete on delete-relaxed task.
It finishes in polynomial time of O(min(facts,ops)).

IW(1) + op-once constraint is not complete in general, but finishes in polynomial time of O(min(facts,ops)).
We sample the state space with a successor function which is randomly ALMOST delete free (randomly semi-relaxed),
and count the number of reaching the semi-relaxed goal.

|#

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))
  (defun alien/rpg (&key
                  (probe *probe-limit*)
                  (semi-relaxation *semi-relaxed-rate-log2*))
    (setf *probe-limit* probe
          *semi-relaxed-rate-log2* semi-relaxation)
    (push 'alien/rpg *optional-features*)
    (push 'ff/rpg *optional-features*)
    (ensure-delete-relaxed-sg nil)
    (ensure-delete-only-sg nil)
    (make-evaluator
     :storage '(list 'alien/rpg)
     :function '(function alien-heuristics/rpg)))
)

(in-compilation-phase ((and alien/rpg phase/packed-structs))
  (alien.lib:define-packed-struct alien/rpg ()
    (value 0 (runtime integer 0 *probe-limit*)))
)

(in-compilation-phase ((and alien/rpg phase/full-compilation))
  (defun delete-only-apply-op (op-id parent child)
    ;; only add some propositions
    (compiled-apply-op op-id parent child *delete-only-ops*)))

(in-compilation-phase ((and alien/rpg phase/full-compilation))
  (ftype* alien-heuristics/rpg state+axioms (runtime integer 0 *probe-limit*))
  (defun alien-heuristics/rpg (state)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((op-db (load-time-value (make-array *delete-relaxed-op-size* :element-type 'bit)))
           (state-db (make-state+axioms))
           (state1 (make-state+axioms))
           (state2 (make-state+axioms))
           (state3 (make-state+axioms))
           (success 0))
      (declare (dynamic-extent state-db state1 state2 state3)
               (fixnum success))
      (dotimes (i (maybe-inline-obj *probe-limit*) (maybe-inline-obj *probe-limit*))
        (fill op-db 0)
        (replace state-db state)
        (replace state1 state)
        (replace state2 state)
        (iter (for step from 0)
              (let ((ops (load-time-value (make-array *op-size* :element-type 'op-id)))
                    (c 0))
                (do-leaf (op-id state1 *delete-relaxed-sg*)
                  (unless (= 1 (aref op-db op-id)) ;else, already applied
                    (setf (aref op-db op-id) 1
                          (aref ops c) op-id)
                    (incf c)))
                (when (zerop c)
                  ;; no new action applied; fail
                  ;; (log:info "took ~a steps (failed, no action)" step)
                  (return))
                (iter (for (the op-id op-id) in-vector ops below c)
                      (delete-relaxed-apply-op op-id state1 state2))
                (apply-axioms state2)
                (when (goalp state2)
                  ;; (log:info "took ~a steps (success)" step)
                  (return-from alien-heuristics/rpg i))

                (let ((achieved (make-state+axioms)))
                  (declare (dynamic-extent achieved))
                  (bit-andc1 state-db state2 achieved) ; 1 when 0 in db and 1 in child
                  (when (not (find 1 achieved))
                    ;; no new proposition added; fail
                    ;; (log:info "took ~a steps (failed, no new proposition)" step)
                    (return)))
                (bit-ior state2 state-db state-db)
                
                ;; apply all deletes
                (replace state3 state2)
                (iter (for (the op-id op-id) in-vector ops below c)
                      (delete-only-apply-op op-id state1 state3))

                ;; restore some deletes
                (let ((deletes (make-state+axioms))
                      (random  (make-state+axioms)))
                  (declare (dynamic-extent deletes random))
                  (bit-andc2 state2 state3 deletes)
                  ;; smaller *semi-relaxed-rate-log2* = more 1s in RANDOM.
                  ;; *semi-relaxed-rate-log2* = 0 is equivalent to delete-relaxation, and meaningless.
                  (set/reset-random-bitvector^2 (maybe-inline-obj *semi-relaxed-rate-log2*) random)
                  ;; choose which deletes to relax.
                  (bit-and deletes random deletes)
                  ;; restore some deletes
                  (bit-ior state3 deletes state3))

                (fill state3 0 :start (maybe-inline-obj *fact-size*))
                (apply-axioms state3)
                ;; prepare for the next iteration
                (replace state1 state3)
                (replace state2 state3))))))
  (print-function-size 'alien-heuristics/rpg))

