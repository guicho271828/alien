
(in-package :strips)

(strips.lib:define-packed-struct eager ()
  (facts 0 state)
  (parent 0 state-id)
  (op -1 op-id)
  (status +new+ status))

(defun eager-search (open-list insert pop)
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
         (child (make-state)))
    (replace state state+axioms)
    (let ((id (close-list-insert close-list state)))
      (funcall insert open-list id state+axioms))
    (setf (state-information-facts info) state
          (state-information-status info) +open+
          (packed-aref db 'state-information 0) info)

    (labels ((rec ()
               (let* ((id (funcall pop open-list)))
                 (packed-aref db 'state-information id info)
                 (when (/= +open+ (state-information-status info))
                   (return-from rec (rec)))
                 (setf (state-information-status info) +closed+)
                 (state-information-facts info state)
                 (replace state+axioms state)
                 (apply-axioms state+axioms)
                 
                 (flet ((path ()
                          (nreverse
                           (iter (for pid initially id then (state-information-parent info))
                                 (packed-aref db 'state-information pid info)
                                 (for op-id = (state-information-op info))
                                 (until (minusp op-id))
                                 (collect (decode-op op-id))))))
                   (declare (dynamic-extent #'path))
                   (report-if-goal state+axioms #'path))
                 
                 (iter (for op-id in-vector (applicable-ops *sg* state+axioms))
                       (for op = (aref *instantiated-ops* op-id))
                       (replace child state)
                       (replace child+axioms state+axioms)
                       (apply-op op state+axioms child+axioms)
                       (apply-axioms child+axioms)
                       (let ((id2 (close-list-insert close-list child)))
                         (packed-aref db 'state-information id2 info)
                         (when (= +new+ (state-information-status info))
                           (setf (state-information-parent info) id
                                 (state-information-op info) op-id
                                 (state-information-status info) +open+
                                 (packed-aref db 'state-information id2) info)
                           (funcall insert open-list id2 child+axioms)))))
               (rec)))
      ;; (declare (inline rec))
      ;; loop unrolling
      (rec))))

(defun eager (open-list)
  (ematch open-list
    ((open-list storage constructor insert pop)
     (make-searcher
      :storage (strips.lib:merge-packed-struct-layout (list 'eager storage))
      :form `(lambda ()
               (eager-search #',constructor
                             #',insert
                             #',pop))))))
