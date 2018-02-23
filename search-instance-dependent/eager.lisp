
(in-package :strips)

#+strips::phase/packed-structs
(strips.lib:define-packed-struct eager ()
  (facts 0 state)
  (parent 0 state-id)
  (op 0 op-id)
  (status +new+ status))

#+strips::phase/full-compilation
(defun eager-search (open-list insert pop)
  (declare (optimize (speed 3)))
  (let* ((db (make-state-information-array
              (max-state-id)))
         (close-list (make-close-list :key-function
                                      (lambda (id)
                                        (let ((info (packed-aref db 'state-information id)))
                                          (state-information-facts info)))))
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
      (funcall insert open-list id state+axioms))
    (setf (state-information-facts info) state
          (state-information-status info) +open+
          (state-information-op info) (length *instantiated-ops*)
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
                          (declare (optimize (debug 3)))
                          (nreverse
                           (iter (for pid initially id then (state-information-parent info))
                                 (packed-aref db 'state-information pid info)
                                 (for op-id = (state-information-op info))
                                 (until (= op-id (length *instantiated-ops*)))
                                 (collect (decode-op op-id))))))
                   (declare (dynamic-extent #'path))
                   (report-if-goal state+axioms #'path))
                 (incf expanded)
                 
                 (iter (for op-id in-vector (applicable-ops/fast state+axioms))
                       ;; DONE: remove special variable references to *sg* and *instantiated-ops*
                       ;; TODO: constant fold applicable-ops, apply-axioms
                       (for op = (aref (load-time-value *instantiated-ops* t) op-id))
                       (fill child+axioms 0)
                       (replace child+axioms state)
                       (apply-op op state+axioms child+axioms)
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
                           (funcall insert open-list id2 child+axioms)))))
               (rec)))
      ;; (declare (inline rec))
      ;; loop unrolling
      (unwind-protect
           (rec)
        (log:info "expanded:  ~a" expanded)
        (log:info "evaluated: ~a" evaluated)
        (log:info "generated: ~a" (close-list-counter close-list))
        (log:info "eval/sec:  ~a" (/ (float (* internal-time-units-per-second evaluated))
                                     (max 1 (- (get-internal-real-time) start))))))))

#-(or strips::phase/packed-structs strips::phase/full-compilation)
(defun eager (open-list)
  (ematch open-list
    ((open-list storage constructor insert pop)
     (make-searcher
      :storage (cons 'eager storage)
      :form `(lambda ()
               (eager-search #',constructor
                             #',insert
                             #',pop))))))
