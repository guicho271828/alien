
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *memory-limit* 8000000
  "maximum amount of memory in kb")

(defvar *time-limit* 300
  "runtime limit in sec")

(defvar *heuristics* 'ff)

(defvar *open-list* )
