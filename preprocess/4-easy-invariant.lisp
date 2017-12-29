
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
        (for name = (first p))
        (push name (getf result (predicate-monotonicity/predicate name))))
  result)

(defun predicate-monotonicity/predicate (p-name)
  (declare (symbol p-name))
  (match* ((or (iter (for a in *actions*)
                     (ematch a
                       ((plist :effect `(and ,@effects))
                        (dolist (e effects)
                          (match e
                            (`(forall ,_ (when ,_ ,atom))
                              (when (positive atom)
                                (thereis (eq p-name (car atom))))))))))
               (iter (for a in *axioms*)
                     (ematch a
                       ((list :derived `(,name ,@_) _)
                        (thereis (eq name p-name))))))
           (iter (for a in *actions*)
                 (ematch a
                   ((plist :effect `(and ,@effects))
                    (dolist (e effects)
                      (match e
                        (`(forall ,_ (when ,_ ,atom))
                          (when (negative atom)
                            (thereis (eq p-name (caadr atom)))))))))))
    ((t t) :generic)
    ((t nil) :monotonic+)
    ((nil t) :monotonic-)
    ((nil nil) :static)))

(defvar *monotonicity*)

(defun generic-p (p) (member (first p) (getf *monotonicity* :generic)))
(defun monotonic+p (p) (member (first p) (getf *monotonicity* :monotonic+)))
(defun monotonic-p (p) (member (first p) (getf *monotonicity* :monotonic-)))
(defun static-p (p) (member (first p) (getf *monotonicity* :static)))

(defun added-p (p) (or (generic-p p) (monotonic+p p)))
(defun deleted-p (p) (or (generic-p p) (monotonic-p p)))

(defun in-init-p (p) (member (first p) *init* :key #'car))

(defun never-true-p (p) (and (not (in-init-p p)) (not (added-p p))))

(defun could-become-true-p (p) (not (never-true-p p)))

;; easy operator invariants
(defun never-applicable-p (a)
  (ematch a
    ((list :derived _ `(and ,@body))
     (some (conjoin #'positive #'never-true-p) body))
    ((plist :precondition `(and ,@precond))
     (some (conjoin #'positive #'never-true-p) precond))))
