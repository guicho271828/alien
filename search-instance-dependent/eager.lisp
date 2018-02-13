
(in-package :strips)

(strips.lib:define-packed-struct eager ()
  (facts 0 state)
  (parent 0 state-id)
  (op -1 op-id)
  (status +new+ status))

(strips.lib:define-packed-struct state-information (eager))

(import '(strips.lib:packed-aref
          strips.lib:size-of))

(defun eager-search (open-list insert pop)
  (let* ((db (make-state-information-array
              (/ (* 1024 *memory-limit*)
                 (size-of 'state-information))))
         (close-list (make-close-list :key-function
                                      (lambda (id)
                                        (let ((info (packed-aref db 'state-information id)))
                                          (state-information-facts info)))))
         (open-list (funcall open-list))
         (info (make-state-information))
         (state (initialize-init))
         (child (make-state)))
    
    (funcall insert open-list state)
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
                 (apply-axioms state)
                 
                 (flet ((path ()
                          (nreverse
                           (iter (for pid initially id then (state-information-parent info))
                                 (packed-aref db 'state-information pid info)
                                 (for op-id = (state-information-op info))
                                 (until (minusp op-id))
                                 (collect (decode-op op-id))))))
                   (declare (dynamic-extent #'path))
                   (report-if-goal state #'path))
                 
                 (iter (for op-id in-vector (applicable-ops *sg* state))
                       (for op = (aref *instantiated-ops* op-id))
                       (replace child state)
                       (apply-op op state child)
                       (apply-axioms child)
                       (let ((id2 (close-list-insert close-list child)))
                         (packed-aref db 'state-information id2 info)
                         (when (= +new+ (state-information-status info))
                           (setf (state-information-parent info) id
                                 (state-information-op info) op-id
                                 (state-information-status info) +open+
                                 (packed-aref db 'state-information id2) info)
                           (funcall insert open-list child)))))
               (rec)))
      ;; (declare (inline rec))
      ;; loop unrolling
      (rec))))


(defun eager (open-list)
  (match open-list
    ((open-list storage constructor insert pop)
     (make-searcher
      :storage (strips.lib:merge-packed-struct-layout 'eager storage)
      :body `(lambda ()
               (eager-search #',constructor
                             #',insert
                             #',pop))))))
