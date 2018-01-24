
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun eager (evaluator)
  (let* ((close-list (make-close-list))
         (open-list (make-bucket-open-list))
         (init (initialize-init))
         (init-id (register-state close-list init))
         (parent (make-parent-list))
         (generator (make-generator-list))
         (status (make-status-list)))
    (bucket-open-list-insert open-list (funcall evaluator init) init-id)
    (labels ((rec ()
               (let* ((id (bucket-open-list-pop open-list))
                      (state (retrieve-state close-list id)))
                 #+(or)
                 (print-values (values :popping id state))
                 (when (/= +open+ (safe-aref status id))
                   (return-from rec (rec)))
                 (setf (safe-aref status id) +closed+)
                 (apply-axioms state)
                 (restart-bind ((retrieve-path
                                 (lambda ()
                                   (iter (for pid initially id then (aref parent pid))
                                         (for op  = (aref generator pid))
                                         (until (= op most-positive-fixnum))
                                         (collect (decode-op op))))))
                   (report-if-goal state))
                 #+(or)
                 (print (applicable-ops *sg* state))
                 (let ((child (make-state)))
                   (dolist (op (applicable-ops *sg* state))
                     (replace child state)
                     (apply-op op state child)
                     (apply-axioms child)
                     (let ((id2 (register-state close-list child)))
                       (when (= +new+ (safe-aref status id2))
                         (setf (safe-aref parent id2) id
                               (safe-aref generator id2) op
                               (safe-aref status id2) +open+)
                         (bucket-open-list-insert open-list (funcall evaluator child) id2)))
                     #+(or)
                     (progn
                       (sleep 1)
                       (print-values (values state
                                             (decode-state state)
                                             child
                                             (decode-state child)
                                             open-list))))))))
                 (rec))))
      (rec))))

(defun retrieve-path ()
  (invoke-restart (print (find-restart 'retrieve-path))))
