
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)


(defun easy-invariant (info)
  (with-parsed-information info
    (list* :monotonicity (predicate-monotonicity)
           info)))

;; predicates are:
;; generic: both added or deleted
;; monotonic+: only adds
;; monotonic-: only dels
;; static: never added nor deleted

(defun predicate-monotonicity (&aux result)
  (iter (for p in *predicates*)
        (push (first p) (getf result (predicate-monotonicity/predicate p))))
  result)

(defun predicate-monotonicity/predicate (p)
  (match* ((or (iter (for a in *actions*)
                     (ematch a
                       ((plist :effect `(and ,@effects))
                        (thereis (find p (remove-if-not #'positive effects) :key #'first)))))
               (iter (for a in *axioms*)
                     (ematch a
                       ((list :derived `(,name ,@_) _)
                        (thereis (eq name (first p)))))))
           (iter (for a in *actions*)
                 (ematch a
                   ((plist :effect `(and ,@effects))
                    (thereis (find p (remove-if-not #'negative effects) :key #'first))))))
    ((t t) :generic)
    ((t nil) :monotonic+)
    ((nil t) :monotonic-)
    ((nil nil) :static)))

(defun generic-p (p) (member (first p) (getf *monotonicity* :generic)))
(defun monotonic+p (p) (member (first p) (getf *monotonicity* :monotonic+)))
(defun monotonic-p (p) (member (first p) (getf *monotonicity* :monotonic-)))
(defun static-p (p) (member (first p) (getf *monotonicity* :static)))
(defun added-p (p) (or (generic-p p) (monotonic+p p)))
(defun deleted-p (p) (or (generic-p p) (monotonic-p p)))
