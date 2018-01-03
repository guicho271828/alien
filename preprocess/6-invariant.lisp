(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(-> "ipc2011-opt/transport-opt11/p01.pddl"
  %rel
  parse
  easy-invariant
  ground
  print)

(defvar *facts*)
(defvar *ops*)
(defvar *ground-axioms*)

(defun mutex-invariant (info)
  (with-parsed-information3 info
    (trivial-encoding)))


