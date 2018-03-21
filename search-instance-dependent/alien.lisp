(in-package :strips)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))

(defparameter *probe-limit* 10)

(defparameter *semi-relaxed-rate-log2* 2
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
(defun alien ()
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
                   (if (minusp (maybe-inline-obj *semi-relaxed-rate-log2*))
                       (reset-random-bitvector^2 (maybe-inline-obj (abs *semi-relaxed-rate-log2*)) random)
                       (set-random-bitvector^2 (maybe-inline-obj *semi-relaxed-rate-log2*) random))
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
            (let ((chosen -1)
                  (i 0))
              (declare (fixnum chosen i))
              (flet ((successor-novel-p (op-id)
                       ;; do not choose the same op twice
                       (when (= 1 (aref op-db op-id))
                         (return-from successor-novel-p))
                       
                       (successor op-id)
                       
                       ;; choose the op immediately
                       (when (goalp child)
                         (incf success)
                         ;; (log:info "took ~a steps (reached goal)" step)
                         (return))

                       ;; ignore ops not achieving new propositions
                       (let ((achieved (make-state+axioms)))
                         (declare (dynamic-extent achieved))
                         (bit-andc1 state-db child achieved) ; 1 when 0 in db and 1 in child
                         (when (not (find 1 achieved))
                           (return-from successor-novel-p)))
                       
                       ;; choose the op randomly via reservoir sampling with k=1
                       (incf i)
                       (when (= 0 (random i))
                         (setf chosen op-id))))
                (do-leaf (op-id current *sg*)
                  (successor-novel-p op-id)))
              ;; (log:info "you are the chosen one! ~a" chosen)
              (if (minusp chosen)
                  ;; dead end, no novel op chosen
                  (progn
                    ;; (log:info "took ~a steps (failed)" step)
                    (return))
                  (progn
                    (successor chosen)
                    (setf (aref op-db chosen) 1)
                    (bit-ior state-db child state-db)
                    (replace current child)))))))
      
      ;; (log:info "~a / ~a success." success *probe-limit*)
      (- (maybe-inline-obj *probe-limit*) success))))
