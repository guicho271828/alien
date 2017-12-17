
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(strips:with-parsed-information (strips:parse (strips:%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  strips:*types*)

(defun ground (info)
  (with-parsed-information info
    (multiple-value-bind (propositions transitions) (%ground)
      (list* :propositions propositions
             :transitions transitions
             info))))

(defun %ground ()
  ;; 
  )
