(in-package :strips)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

(defparameter *probe-limit* 3)

(declaim (inline set-random-bitvector^2))
(ftype* set-random-bitvector^2 fixnum simple-bit-vector simple-bit-vector)
(defun set-random-bitvector^2 (i bv)
  (declare (optimize (speed 3)))
  (fill bv 0)
  (dotimes (j i bv)
    (dotimes (i (ceiling (length bv) 64))
      (setf (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
            (logior (strips.lib::%packed-accessor-int-unsafe bv 64 (* i 64))
                    (random (expt 2 64)))))))

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
                 (fill current 0 :start (maybe-inline-obj *fact-size*))
                 (apply-op/fast op-id current child)
                 (let ((deletes (make-state+axioms))
                       (random  (make-state+axioms)))
                   (declare (dynamic-extent deletes random))
                   (bit-andc2 current child deletes)
                   (fill deletes 0 :start (maybe-inline-obj *fact-size*))
                   ;; (break+ current child deletes facts (bit-ior child deletes))
                   ;; randomly semi-delete-relax
                   (set-random-bitvector^2 4 random)
                   (bit-and deletes random deletes)
                   ;; restore some deletes
                   (bit-ior child deletes child))
                 (apply-axioms child)))
        (dotimes (i (maybe-inline-obj *probe-limit*))
          ;; initializing probe
          (replace current state)
          (fill op-db 0)
          (fill state-db 0)
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
                         (return))

                       ;; ignore ops not achieving new propositions
                       (let ((achieved (make-state+axioms)))
                         (declare (dynamic-extent achieved))
                         (bit-andc1 state-db child achieved) ; 1 when 0 in db and 1 in child
                         (when (not (find 1 achieved)) ; when some new propositon is found
                           (return-from successor-novel-p)))
                       
                       ;; choose the op randomly via reservoir sampling with k=1
                       (incf i)
                       (when (= 0 (random i))
                         (setf chosen op-id))))
                (declare (dynamic-extent #'successor-novel-p))
                (do-leaf (op-id current *sg*)
                  (successor-novel-p op-id)))
              (if (plusp chosen)
                  (successor chosen)
                  ;; dead end, no novel op chosen
                  (return))))))
      
      ;; (format t "~& ~a / ~a success." success *probe-limit*)
      (- (maybe-inline-obj *probe-limit*) success))))
