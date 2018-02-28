
(in-package :strips)

(in-compilation-phase ((and eager phase/packed-structs))
(strips.lib:define-packed-struct eager ()
  (facts 0 state)
  (parent 0 state-id)
  (op 0 op-id)
  (status +new+ status)))

(in-compilation-phase ((and eager phase/full-compilation))
  (defun print-plan-simulation (op-ids)
    (log:debug
     "~a"
     (with-output-to-string (s)
       (iter (for op-id in op-ids)
             (for i from 0) 
             (with state+axioms = (initial-state+axioms))
             (with child+axioms = (initial-state+axioms))
             (when (first-iteration-p)
               (format s "~3%      ~a~%" state+axioms)
               (format s "      ~a~%" (decode-state state+axioms)))
             
             (apply-op/fast op-id state+axioms child+axioms)
             (apply-axioms child+axioms)

             (format s "~a : ~a, id: ~a~%" i (decode-op op-id) op-id)
             (let ((*package* (find-package :pddl)))
               (format s "     ~s~%" (match (strips.lib:index-ref *op-index* op-id)
                                       ((list (list* name _) _)
                                        (find name *actions* :key #'second)))))
             (format s "     ~a~%" (aref *instantiated-ops* op-id))
             (format s "      ~a~%" state+axioms)
             (format s "      ~a~%" child+axioms)
             (iter (for b1 in-vector state+axioms with-index i)
                   (for b2 in-vector child+axioms)
                   (match* (b1 b2)
                     ((0 0))
                     ((1 0) (format s "      ~3a : -~a~%" i (decode-fact i)))
                     ((0 1) (format s "      ~3a : +~a~%" i (decode-fact i)))
                     ((1 1) (format s "      ~3a :  ~a~%" i (decode-fact i)))))
             
             (format s "      other conditions~%")

             ;; others that are mentioned
             (let ((all (bit-ior state+axioms child+axioms))
                   acc)
               (match (aref *instantiated-ops* op-id)
                 ((op pre eff)
                  (iter (for i in-vector pre) (push (logabs i) acc))
                  (iter outer
                        (for e in-vector eff)
                        (match e
                          ((effect con eff)
                           (iter (for i in-vector con) (push (logabs i) acc))
                           (push (logabs eff) acc))))))
               (dolist (var (remove-duplicates acc))
                 (ignore-errors
                   (unless (= 1 (aref all var))
                     (format s "      ~3a : ~a~%" var (decode-fact var))))))
             
             (replace state+axioms child+axioms))))))

(in-compilation-phase ((and eager phase/full-compilation))
(defun eager-search (open-list insert pop)
  (declare (optimize (speed 3)))
  (let* ((db (make-state-information-array
              (max-state-id)))
         (close-list (make-close-list :key-function
                                      (let ((info (make-state-information))
                                            (state (make-state)))
                                        (lambda (id)
                                          (packed-aref db 'state-information id info)
                                          (state-information-facts info state)))))
         (open-list (funcall open-list))
         (info (make-state-information))
         (state+axioms (initial-state+axioms))
         (child+axioms (make-state+axioms))
         (state (make-state))
         (child (make-state))
         (expanded 0)
         (evaluated 1)
         (start (get-internal-real-time)))
    (declare (fixnum expanded evaluated start))
    (replace state state+axioms)
    (let ((id (close-list-insert close-list state)))
      (log:info "Initial heuristic value: ~a"
                (funcall insert open-list id state+axioms)))
    (setf (state-information-facts info) state
          (state-information-status info) +open+
          (state-information-op info) *op-size*
          (packed-aref db 'state-information 0) info)

    (labels ((rec ()
               (let* ((id (funcall pop open-list)))
                 (packed-aref db 'state-information id info)
                 (when (/= +open+ (state-information-status info))
                   (return-from rec (rec)))
                 (setf (state-information-status info) +closed+)
                 (state-information-facts info state)
                 (fill state+axioms 0)
                 (replace state+axioms state)
                 (apply-axioms state+axioms)
                 
                 (flet ((path ()
                          (log:info "Goal found!")
                          (let ((op-ids (nreverse
                                         (iter (for pid initially id then (state-information-parent info))
                                               (packed-aref db 'state-information pid info)
                                               (for op-id = (state-information-op info))
                                               (until (= op-id *op-size*))
                                               (collect op-id)))))
                            (print-plan-simulation op-ids)
                            (mapcar #'decode-op op-ids))))
                   (declare (dynamic-extent #'path))
                   (report-if-goal state+axioms #'path))
                 (incf expanded)

                 (multiple-value-bind (ops len) (applicable-ops/fast state+axioms)
                 (iter (for op-id in-vector ops with-index i below len)
                       ;; DONE: remove special variable references to *sg* and *instantiated-ops*
                       ;; TODO: constant fold applicable-ops, apply-axioms
                       (fill child+axioms 0)
                       (replace child+axioms state)
                       (apply-op/fast op-id state+axioms child+axioms)
                       (apply-axioms child+axioms)
                       (replace child child+axioms)
                       
                       (let ((id2 (close-list-insert close-list child)))
                         (packed-aref db 'state-information id2 info)
                         (when (= +new+ (state-information-status info))
                           (incf evaluated)
                           (setf (state-information-facts info) child
                                 (state-information-parent info) id
                                 (state-information-op info) op-id
                                 (state-information-status info) +open+
                                 (packed-aref db 'state-information id2) info)
                           (funcall insert open-list id2 child+axioms))))))
               (rec)))
      ;; (declare (inline rec))
      ;; loop unrolling
      (unwind-protect
           (rec)
        (log:info "expanded:  ~a" expanded)
        (log:info "evaluated: ~a" evaluated)
        (log:info "generated: ~a" (close-list-counter close-list))
        (log:info "eval/sec:  ~a" (/ (float (* internal-time-units-per-second evaluated))
                                     (max 1 (- (get-internal-real-time) start)))))))))

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))
(defun eager (open-list)
  (push 'eager *optional-features*)
  (ematch open-list
    ((open-list storage constructor insert pop)
     (make-searcher
      :storage (cons 'eager storage)
      :form `(lambda ()
               (eager-search #',constructor
                             #',insert
                             #',pop))))))
)
