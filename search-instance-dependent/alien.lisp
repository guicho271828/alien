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
    (let* ((current  (load-time-value (make-state+axioms)))
           (child    (load-time-value (make-state+axioms)))
           (success  0)
           (op-db    (load-time-value (make-array *op-size* :element-type 'bit)))
           (state-db (load-time-value (make-state+axioms))))
      (labels ((successor (op-id)
                 (let ((facts (load-time-value (fill (make-state+axioms) 1 :end *fact-size*))))
                   (bit-and current facts child)
                   (apply-op/fast op-id current child)
                   (let ((deletes (load-time-value (make-state+axioms)))
                         (random  (load-time-value (make-state+axioms))))
                     (bit-andc2 current child deletes)
                     (bit-and deletes facts deletes)
                     ;; (break+ current child deletes facts (bit-ior child deletes))
                     ;; randomly semi-delete-relax
                     (set-random-bitvector^2 4 random)
                     (bit-and deletes random deletes)
                     ;; restore some deletes
                     (bit-ior child deletes child))
                   (apply-axioms child)))
               
               (move-to-novel-successor (op-id)
                 (when (= 1 (aref op-db op-id))
                   ;; do not choose the same op
                   (return-from move-to-novel-successor 0))
                 
                 (successor op-id)
                 
                 (when (goalp child)
                   (incf success)
                   (return-from move-to-novel-successor 2))

                 (let ((achieved (load-time-value (make-state+axioms))))
                   ;; 1 when 0 in db and 1 in child
                   (bit-andc1 state-db child achieved)
                   ;; when some new propositon is found
                   (when (find 1 achieved)
                     ;; choose the op immediately.
                     ;; mark the op so that it wont be selected again
                     (setf (aref op-db op-id) 1)
                     (replace current child)
                     (return-from move-to-novel-successor 1)))
                 ;; (format t "~&step ~A: no novel action applicable, dead end; failure" step)
                 (return-from move-to-novel-successor 2)))
        (iter (declare (declare-variables))
              (for i below *probe-limit*)
              (declare (fixnum i))
              ;; initialization
              (replace current state)
              (fill op-db 0)
              (fill state-db 0)
              (iter (declare (declare-variables))
                    (for step from 0 below *op-size*)
                    (declare (fixnum step))
                    (do-leaf (op-id current *sg*)
                      (case (move-to-novel-successor op-id)
                        (0 )
                        (1 (next-iteration))
                        (2 (finish)))))))
              
        ;; (format t "~& ~a / ~a success." success *probe-limit*)
        (- *probe-limit* success))))
