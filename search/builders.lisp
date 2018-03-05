

(in-package :strips)

(defun alien ()
  (push 'alien *optional-features*)
  (push 'novelty1 *optional-features*)
  (push 'novelty2 *optional-features*)
  (push 'novelty3 *optional-features*)
  (ensure-random-semi-delete-relaxed-sg)
  (make-evaluator
   :storage '()
   :function '(function alien-heuristics)))

(defun eager (open-list)
  (push 'eager *optional-features*)
  (ematch open-list
    ((open-list storage constructor insert pop)
     (make-searcher
      :storage (cons 'eager storage)
      :form `(lambda ()
               (eager-search #',constructor
                             #',insert
                             #',pop))))))

(defun ff/rpg ()
  (ensure-delete-relaxed-sg)
  (push 'ff/rpg *optional-features*)
  (make-evaluator
   :storage '() ; no cache
   :function '(function ff-heuristic/rpg)))

(defun goal-count ()
  (push 'goal-count *optional-features*)
  (make-evaluator
   :storage '(goal-count)
   :function '(function goal-count-heuristics)))

(defun novelty (&rest keys
                &key (k most-positive-fixnum)
                  (initial-num-slots 256)
                  (cache-size 262144)
                  (max-memory 0))
  (declare (ignorable k initial-num-slots cache-size max-memory))
  (push 'novelty *optional-features*)
  (make-evaluator
   :storage '()
   :function `(load-time-value (make-novelty-heuristics ,@keys) t)))

(defun novelty1 ()
  (push 'novelty1 *optional-features*)
  (make-evaluator
   :storage '()
   :function '(load-time-value (make-novelty1-heuristics) t)))

(defun novelty2 ()
  (push 'novelty2 *optional-features*)
  (make-evaluator
   :storage '()
   :function '(load-time-value (make-novelty2-heuristics) t)))

(defun novelty3 ()
  (push 'novelty3 *optional-features*)
  (make-evaluator
   :storage '()
   :function '(load-time-value (make-novelty3-heuristics) t)))

(defun novelty4 ()
  (push 'novelty4 *optional-features*)
  (make-evaluator
   :storage '()
   :function '(load-time-value (make-novelty4-heuristics) t)))

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

