
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun eager (evaluator)
  (let* ((close-list (make-close-list))
         (open-list (make-bucket-open-list))
         (init (initialize-init))
         (status (make-status-list)))
    (bucket-open-list-insert open-list (funcall evaluator init) (register-state close-list init))
    (labels ((rec ()
               (let* ((id (bucket-open-list-pop open-list))
                      (state (retrieve-state close-list id)))
                 (print-values (values :popping id state))
                 (if (/= +open+ (aref status id))
                     (rec)
                     (setf (safe-aref status id) +closed+))
                 (apply-axioms state)
                 (report-if-goal state)
                 (print (applicable-ops *sg* state))
                 (let ((child (make-state)))
                   (dolist (op (applicable-ops *sg* state))
                     (replace child state)
                     (apply-op op state child)
                     (apply-axioms child)
                     (let ((id (register-state close-list child)))
                       (bucket-open-list-insert open-list (funcall evaluator child) id))
                     ;; #+(or)
                     (progn
                       (sleep 1)
                       (print-values (values state
                                             (decode-state state)
                                             child
                                             (decode-state child)
                                             open-list)))))
                 (rec))))
      (rec))))
