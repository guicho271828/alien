
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips.lib:define-packed-struct eager ()
  (facts 0 (runtime simple-bit-vector *fact-size*))
  (parent 0 (unsigned-byte 32))
  (generator 0 (runtime unsigned-byte (length *instantiated-ops*)))
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
                 (setf info (packed-aref db 'state-information id))
                 (when (/= +open+ (state-information-status info))
                   (return-from rec (rec)))
                 (setf (state-information-status info) +closed+)
                 (setf state (state-information-fact info))
                 (apply-axioms state)
                 (report-if-goal state)
                 (iter (for op in-vector (applicable-ops *sg* state))
                       (replace child state)
                       (apply-op op state child)
                       (apply-axioms child)
                       (let ((id2 (register-state close-list child)))
                         (setf info (packed-aref db 'state-information id))
                         (when (= +new+ (state-information-status info))
                           (setf (state-information-parent info) id
                                 (state-information-generator info) op
                                 (state-information-generator info) +open+
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
