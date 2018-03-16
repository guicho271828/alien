(in-package :strips)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))

(defparameter *probe-limit* 10)

(defparameter *semi-relaxed-rate-log2* 3
  "Specifies the delete-relaxation rate in the search space explored by the probes.
0 means that all delete effects are relaxed.

1 means 1/2 the delete effects are relaxed, rest are preserved.
2 means 3/4, 3 means 7/8, 4 means 15/16 and so on (the problem is almost delete-relaxed).

When the value is negative, it uses an alternative scheme.
-1 means 1/2 the delete effects are relaxed, rest are preserved.
-2 means 1/4, -3 means 1/8, -4 means 1/16 and so on (the problem is almost similar to the original).

When this value is set to 0, it means the probe searches in a fully delete-relaxed state space.
In this case, the probe is complete and always find a goal when the original problem is solvable,
and fails otherwise. This means that the outcome of sampling (random exploration) is deterministic.

In order to bring some nondeterminism and conduct a meaningful sampling,
 you should set a non-zero value.
")

(declaim (inline set-random-bitvector^2))
(ftype* set-random-bitvector^2 fixnum simple-bit-vector simple-bit-vector)
(defun set-random-bitvector^2 (i bv)
  "Randomly set a bit vector BV according to the integer I.
Approximately 1/2^I of the bits are randomly selected and set to 0, and the rest are 1.
Larger I means more 1s.
 
Examples:
When I = 0, bv is filled with 0.
When I = 1, approximately half the bits are 0.
When I = 2, approximately 1/4 of the bits are 0."
  (declare (optimize (speed 3)))
  (fill bv 0)
  (dotimes (j i bv)
    (dotimes (i (ceiling (length bv) 64))
      (setf (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
            (logior (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
                    (random (expt 2 64)))))))

(declaim (inline reset-random-bitvector^2))
(ftype* reset-random-bitvector^2 fixnum simple-bit-vector simple-bit-vector)
(defun reset-random-bitvector^2 (i bv)
  "Randomly reset a bit vector BV according to the integer I.
Approximately 1/2^I of the bits are randomly selected and set to 1, and the rest are 0.
Larger I means more 0s.
 
Examples:
When I = 0, bv is filled with 0.
When I = 1, approximately half the bits are 0.
When I = 2, approximately 1/4 of the bits are 0."
  (declare (optimize (speed 3)))
  (fill bv 1)
  (dotimes (j i bv)
    (dotimes (i (ceiling (length bv) 64))
      (setf (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
            (logand (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
                    (random (expt 2 64)))))))

(declaim (inline set/reset-random-bitvector^2))
(ftype* set/reset-random-bitvector^2 fixnum simple-bit-vector simple-bit-vector)
(defun set/reset-random-bitvector^2 (i bv)
  (declare (optimize (speed 3)))
  (if (plusp i)
      (set-random-bitvector^2 i bv)
      (reset-random-bitvector^2 i bv)))

)

;; e.g.
;; (strips::set-random-bitvector^2 4 #*0000000000000000000000000000000000000000000000)
;; #*1111111111111111101111110111111111111111111111

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
  (defun alien (&key
                  (probe *probe-limit*)
                  (semi-relaxation *semi-relaxed-rate-log2*))
    (setf *probe-limit* probe
          *semi-relaxed-rate-log2* semi-relaxation)
    (push 'alien *optional-features*)
    (make-evaluator
     :storage '()
     :function '(function alien-heuristics)))
)

(in-compilation-phase ((and alien phase/full-compilation))
  (ftype* alien-heuristics state+axioms (runtime integer 0 *probe-limit*))
  (defun alien-heuristics (state)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((current  (make-state+axioms))
           (child    (make-state+axioms))
           (success  0)
           (op-db    (load-time-value (make-array *op-size* :element-type 'bit)))
           (state-db (make-state+axioms)))
      (declare (dynamic-extent current child state-db)
               ((runtime integer 0 *probe-limit*) success))
      (labels ((successor (op-id)
                 (replace child current :end2 (maybe-inline-obj *fact-size*))
                 (fill child 0 :start (maybe-inline-obj *fact-size*))
                 (apply-op/fast op-id current child)
                 (let ((deletes (make-state+axioms))
                       (random  (make-state+axioms)))
                   (declare (dynamic-extent deletes random))
                   (bit-andc2 current child deletes)
                   ;; does not restore axioms
                   (fill deletes 0 :start (maybe-inline-obj *fact-size*))
                   ;; smaller *semi-relaxed-rate-log2* = more 1s in RANDOM.
                   ;; *semi-relaxed-rate-log2* = 0 is equivalent to delete-relaxation, and meaningless.
                   (set/reset-random-bitvector^2 (maybe-inline-obj *semi-relaxed-rate-log2*) random)
                   ;; choose which deletes to relax.
                   (bit-and deletes random deletes)
                   ;; restore some deletes
                   (bit-ior child deletes child))
                 (apply-axioms child)))
        (dotimes (i (maybe-inline-obj *probe-limit*))
          ;; initializing probe
          (replace current state)
          (fill op-db 0)
          (fill state-db 0)
          ;; (log:info "new probe")
          (dotimes (step (maybe-inline-obj *op-size*))
            (let ((ops (load-time-value (make-array *op-size* :element-type 'op-id)))
                  (c 0)
                  (chosen -1))
              (declare (fixnum c chosen))
              (do-leaf (op-id current *sg*)
                (when (= 0 (aref op-db op-id))
                  (setf (aref ops c) op-id)
                  (incf c)))
              ;; randomize selection
              (shuffle ops :end c)
              
              (iter choose-op
                    (declare (declare-variables))
                    (for (the op-id op-id) in-vector ops with-index i below c)

                    (successor op-id)   ;this is a heavy operation, calls to it should be minimized
                    (when (goalp child) ;when goal, terminate the probe immediately
                      (incf success)
                      ;; (log:info "took ~a steps (reached goal)" step)
                      (return))
                    
                    ;; skip the op not achieving new propositions
                    (let ((achieved (make-state+axioms)))
                      (declare (dynamic-extent achieved))
                      (bit-andc1 state-db child achieved) ; 1 when 0 in db and 1 in child
                      (when (not (find 1 achieved))
                        ;; skip the op.
                        ;; Also, since novelty is monotonic, there is no chance that this op
                        ;; would achive new propositions in the next iteration. Thus, we can
                        ;; safely remove this op from the consideration.
                        (setf (aref op-db op-id) 1)
                        (next-iteration)))

                    ;; the op satisfies all conditions.
                    (setf chosen op-id)
                    (finish))

              ;; (log:info "you are the chosen one! ~a" chosen)
              (if (minusp chosen)
                  ;; dead end, no novel op chosen
                  (progn
                    ;; (log:info "took ~a steps (failed)" step)
                    (return))
                  (progn
                    (setf (aref op-db chosen) 1)
                    (bit-ior state-db child state-db)
                    (replace current child)))))))
      
      ;; (log:info "~a / ~a success." success *probe-limit*)
      (- (maybe-inline-obj *probe-limit*) success)))
  (print-function-size 'alien-heuristics))
