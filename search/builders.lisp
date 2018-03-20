

(in-package :strips)

(defun sum (&rest evaluators)
  (with-gensyms (sum)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,sum ()
                       (value 0 (runtime integer 0
                                         (reduce #'+
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key (lambda (struct) (expt 2 (size-of struct))))))))
     :function `(lambda (state)
                  (+ ,@(mapcar
                        (lambda (e)
                          `(funcall ,(evaluator-function e) state))
                        evaluators))))))

(defun product (&rest evaluators)
  (with-gensyms (product)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,product ()
                       (value 0 (runtime unsigned-byte
                                         ;; because the size of storages are unknown when
                                         ;; the builders are evaluated
                                         (reduce #'+
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key #'size-of)))))
     :function `(lambda (state)
                  (* ,@(mapcar
                        (lambda (e)
                          `(funcall ,(evaluator-function e) state))
                        evaluators))))))

(defun maximum (&rest evaluators)
  (with-gensyms (maximum)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,maximum ()
                       (value 0 (runtime unsigned-byte
                                         (reduce #'max
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key #'size-of
                                                 :initial-value 0)))))
     :function `(lambda (state)
                  (max ,@(mapcar
                          (lambda (e)
                            `(funcall ,(evaluator-function e) state))
                          evaluators))))))

(defun constant (value)
  (assert (integerp value))
  (with-gensyms (constant)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,constant ()
                       (value 0 (integer ,value ,value))))
     :function `(constantly ,value))))

(defun shift-for (max-value evalautor)
  (product
   (constant (expt 2 (ceiling (log max-value 2))))
   evalautor))

(defun threshold (threshold evaluator &key (except-init t))
  (assert (integerp threshold))
  (make-evaluator
   :storage (evaluator-storage evaluator)
   :function `(lambda (state)
                (let ((value (funcall ,(evaluator-function evaluator) state))
                      ,@(when except-init `((init t))))
                  (if (and ,@(when except-init `((not init))) (< ,threshold value))
                      (throw 'prune t)
                      (progn
                        ,@(when except-init `((setf init nil)))
                        value))))))

