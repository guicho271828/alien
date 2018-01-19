
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *memory-limit* 1024 ; 1MB
  "maximum amount of memory in kB")

(defvar *time-limit* 300
  "runtime limit in sec")

(defvar *heuristics* 'ff)

