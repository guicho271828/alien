(in-package :strips)

#|

Performs some blind search on the semi-relaxed state space, then
count the number of cases successfully reaching the goal.

|#

(defvar *probe-limit* 100)

(in-compilation-phase ((and alien phase/full-compilation))
  (ftype* alien-heuristics state+axioms (runtime integer 0 *probe-limit*))
  (defun alien-heuristics (state)
    (let* ((ops      (load-time-value (make-array *op-size* :element-type 'op-id)))
           (minh-ops (load-time-value (make-array *op-size* :element-type 'op-id)))
           (hvalues  (load-time-value (make-array *op-size* :element-type 'fixnum)))
           (current  (load-time-value (make-state+axioms)))
           (child    (load-time-value (make-state+axioms)))
           (success  0)
           (k 3)
           (db (load-time-value
                (make-array (list *state-size* *state-size*)
                            :element-type 'bit
                            :initial-element 0)))
           (%db (sb-kernel:%array-data db)))
      (declare ((runtime simple-bit-vector (* *state-size* *state-size*)) %db))
      (flet ((h (state) (novelty2-heuristics state db))
             (h* (state) (novelty2-heuristics* state db))
             (successor (op-id)
               (fill child 0)
               (replace child current :end1 (maybe-inline-obj *fact-size*))
               (compiled-apply-op op-id current child *random-semi-delete-relaxed-ops*)
               (apply-axioms child)))
        
        (iter rollout
              (for i below *probe-limit*)
              (fill %db 0)
              (replace current state)
              ;; (format t "~&~a th probe from ~a" i state)
              (iter novelty-hill-climbing
                    (for step from 0)
                    (for c = 0)
                    (do-leaf (op-id current *random-semi-delete-relaxed-sg*)
                      (setf (aref ops c) op-id)
                      (incf c))

                    ;; (format t "~&~a successors" c)
                    (for minh = k)
                    (for minh-count = 0)
                    
                    (iter find-best
                          (for op-id in-vector ops with-index i below c)
                          (successor op-id)
                          
                          (when (goalp child)
                            ;; (format t "~&step ~a: goal found: success." step)
                            (incf success)
                            (return-from novelty-hill-climbing))

                          (let ((hval (h* child)))
                            (setf (aref hvalues i) hval)

                            (cond
                              ((< hval minh)
                               (setf minh-count 1
                                     minh hval))
                              ((= hval minh)
                               (incf minh-count)))))

                    (when (= minh 3)
                      ;; (format t "~&step ~A: no novel action applicable, dead end; failure" step)
                      (return-from novelty-hill-climbing))
                    
                    ;; (format t "~&~a successors, ~a best novelty successors, novelty ~a" c minh-count minh)
                    
                    (for selected = (random minh-count))
                    ;; (format t "~&selected ~a th" selected)
                    
                    (successor
                     (iter (for h in-vector hvalues with-index i below c)
                           (with count = 0)
                           (when (= h minh)
                             (if (= count selected)
                                 (return (aref ops i))
                                 (incf count)))))
                    
                    ;; (format t "~&next state: ~a" child)
                    ;; (format t "~&updating db:~%~a" %db)

                    ;; update db
                    (h child)
                    ;; (format t "~&~a" %db)

                    (replace current child)))
        (format t "~& ~a / ~a success." success *probe-limit*)
        (- *probe-limit* success)))))
