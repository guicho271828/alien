

(in-package :strips)

(defun sum (&rest evaluators)
  (make-evaluator
   :storage (mappend #'evaluator-storage evaluators)
   :function `(lambda (state)
                (+ ,@(mapcar
                      (lambda (e)
                        `(funcall ,(evaluator-function e) state))
                      evaluators)))))

(defun product (&rest evaluators)
  (make-evaluator
   :storage (mappend #'evaluator-storage evaluators)
   :function `(lambda (state)
                (* ,@(mapcar
                      (lambda (e)
                        `(funcall ,(evaluator-function e) state))
                      evaluators)))))

(defun maximum (&rest evaluators)
  (make-evaluator
   :storage (mappend #'evaluator-storage evaluators)
   :function `(lambda (state)
                (max ,@(mapcar
                        (lambda (e)
                          `(funcall ,(evaluator-function e) state))
                        evaluators)))))

(defun constant (value)
  (assert (integerp value))
  (make-evaluator
   :storage nil
   :function `(constantly ,value)))

(defun shift-for (max-value evalautor)
  (product
   (constant (expt 2 (ceiling (log max-value 2))))
   evalautor))

(defun threshold (threshold evaluator &key (except-init t))
  (assert (integerp threshold))
  (make-evaluator
   :storage nil
   :function `(lambda (state)
                (let ((value (funcall ,(evaluator-function evaluator) state))
                      ,@(when except-init `((init t))))
                  (if (and ,@(when except-init `((not init))) (< ,threshold value))
                      (throw 'prune t)
                      (progn
                        ,@(when except-init `((setf init nil)))
                        value))))))

