
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips.lib:define-packed-struct eager ()
  (facts 0 (runtime simple-bit-vector *fact-size*))
  (parent 0 state-id)
  (op -1 (runtime integer -1 (length *instantiated-ops*)))
  (status +new+ status))

(strips.lib:define-packed-struct state-information (eager))

(defun eager-search (open-list insert pop)
  (let* ((db (make-state-information-array
              (/ (* 1024 *memory-limit*)
                 (size-of 'state-information))))
         (close-list (make-close-list))
         (open-list (funcall open-list))
         (info (make-state-information))
         (state (initialize-init))
         (child (make-state)))
    
    (funcall insert open-list init)
    (setf (state-information-fact info) state
          (state-information-status info) +open+
          (packed-aref db 'state-information 0) info)

    (labels ((rec ()
               (let* ((id (funcall pop open-list)))
                 (packed-aref db 'state-information id info)
                 (when (/= +open+ (state-information-status info))
                   (return-from rec (rec)))
                 (setf (state-information-status info) +closed+)
                 (setf state (state-information-fact info state))
                 (apply-axioms state)
                 
                 (flet ((path ()
                          (nreverse
                           (iter (for pid initially id then (state-information-parent info))
                                 (packed-aref db 'state-information pid info)
                                 (for op = (state-information-op info))
                                 (until (minusp op))
                                 (collect (decode-op op))))))
                   (declare (dynamic-extent #'path))
                   (report-if-goal state #'path))
                 
                 (iter (for op in-vector (applicable-ops *sg* state))
                       (replace child state)
                       (apply-op op state child)
                       (apply-axioms child)
                       (let ((id2 (close-list-insert close-list child)))
                         (packed-aref db 'state-information id2 info)
                         (when (= +new+ (state-information-status info))
                           (setf (state-information-parent info) id
                                 (state-information-op info) op
                                 (state-information-status info) +open+
                                 (packed-aref db 'state-information id2) info)
                           (funcall insert open-list init)))))
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
