
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
  (let ((add (iter outer
                   (for a in *actions*)
                   (ematch a
                     ((plist :effect `(and ,@effects))
                      (dolist (e effects)
                        (ematch e
                          (`(forall ,_ (when ,_ ,atom))
                            ;; because DOLIST establishes a NIL block
                            ;; and THEREIS clause gets confused 
                            (when (and (positive atom) (eq p-name (car atom)))
                              (return-from outer t)))))))))
        (axiom (iter (for a in *axioms*)
                     (ematch a
                       ((list :derived `(,name ,@_) _)
                        (thereis (eq name p-name))))))
        (del (iter outer
                   (for a in *actions*)
                   (ematch a
                     ((plist :effect `(and ,@effects))
                      (dolist (e effects)
                        (ematch e
                          (`(forall ,_ (when ,_ ,atom))
                            (when (and (negative atom) (eq p-name (caadr atom)))
                              (return-from outer t))))))))))
    (ematch* ((or add axiom) del)
      ((t t) :generic)
      ((t nil) :monotonic+)
      ((nil t) :monotonic-)
      ((nil nil) :static))))

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
    ((plist :precondition nil)
     nil)
    ((plist :precondition `(and ,@precond))
     (some (conjoin #'positive #'never-true-p) precond))))

(defun axiom-p (p) (find (first p) *axioms* :key #'caadr))

