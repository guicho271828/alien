
(in-package :strips)

(defvar *enable-no-op-pruning* nil)

(defvar *enable-negative-precondition-pruning-for-fluents* nil)
(defvar *enable-negative-precondition-pruning-for-axioms* nil)

(define-symbol-macro *enable-negative-precondition-pruning*
    (or *enable-negative-precondition-pruning-for-axioms*
        *enable-negative-precondition-pruning-for-fluents*))
