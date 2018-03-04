

(in-package :strips)

(defun alien ()
  (push 'alien *optional-features*)
  (ensure-delete-relaxed-sg)
  (make-evaluator
   :storage '(alien)
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

(defun novelty ()
  (push 'novelty *optional-features*)
  (make-evaluator
   :storage '(novelty)
   :function 'novelty-heuristics))
