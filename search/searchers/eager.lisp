
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(ftype* eager searcher)
(defun eager ()
  (let* ((state (initialize-init))
         (open (make-bucket-open-list))
         (init-id (register-state state)))

    
    (do* ((id init-id (bucket-open-list-pop open))
          (state state (retrieve-state id state)))
         ()
      
      (apply-axioms state)
      (report-if-goal state)
      (let ((child (make-temporary-state)))
        (dolist (op (applicable-ops *sg* state))
          (apply-op (aref *instantiated-ops* op)
                    state child)
          (apply-axioms child)
          (let ((id (register-state child)))
            (bucket-open-list-insert open (evaluate child) id)))))))


    
  
