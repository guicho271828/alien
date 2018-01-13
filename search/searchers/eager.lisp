
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun eager (evaluator)
  (let* ((init (initialize-init))
         (open (make-bucket-open-list)))
    
    (do* ((id (register-state init) (bucket-open-list-pop open))
          (state init (retrieve-state id)))
         (nil)
      
      (apply-axioms state)
      (report-if-goal state)
      
      (let ((child (make-state t)))
        (dolist (op (applicable-ops *sg* state))
          (replace child state)
          (apply-op op state child)
          (apply-axioms child)
          (let ((id (register-state child)))
            (bucket-open-list-insert open (funcall evaluator child) id)))))))

  
