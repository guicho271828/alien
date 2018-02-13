
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips.lib:define-packed-struct eager ()
  (parent 0 (runtime unsigned-byte *fact-size*))
  (generator 0 (runtime unsigned-byte (length *instantiated-ops*)))
  (status +new+ status))

(defun eager-search (evaluator)
  (let* ((close-list (make-close-list))
         (open-list (make-bucket-open-list))
         (init (initialize-init))
         (init-id (register-state close-list init))
         (parent (make-parent-list))
         (generator (make-generator-list))
         (status (make-status-list)))
    (bucket-open-list-insert open-list (funcall evaluator init) init-id)
    (setf (safe-aref status init-id +new+) +open+)
    (labels ((rec ()
               (let* ((id (bucket-open-list-pop open-list))
                      (state (retrieve-state close-list id)))
                 #+(or)
                 (print-values (values :popping id state (decode-state state)
                                       (when-let ((op (aref generator id)))
                                         (decode-op op))))
                 (when (/= +open+ (safe-aref status id +new+))
                   (return-from rec (rec)))
                 (setf (safe-aref status id +new+) +closed+)
                 (apply-axioms state)
                 (restart-bind ((retrieve-path
                                 (lambda ()
                                   (nreverse
                                    (iter (for pid initially id then (aref parent pid))
                                          (for op = (aref generator pid))
                                          (until (null op))
                                          (collect (decode-op op)))))))
                   (report-if-goal state))
                 #+(or)
                 (println (map 'list #'decode-op (applicable-ops *sg* state)))
                 (let ((child (make-state)))
                   (iter (for op in-vector (applicable-ops *sg* state))
                         (replace child state)
                         (apply-op op state child)
                         (apply-axioms child)
                         (let ((id2 (register-state close-list child)))
                           (when (= +new+ (safe-aref status id2 +new+))
                             (setf (safe-aref parent id2) id
                                   (safe-aref generator id2) op
                                   (safe-aref status id2 +new+) +open+)
                             (bucket-open-list-insert open-list (funcall evaluator child) id2)))
                         #+(or)
                         (progn
                           (sleep 0.1)
                           (print-values (values state
                                                 (decode-state state)
                                                 child
                                                 (decode-state child)
                                                 open-list))
                           ))))
               (rec)))
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
