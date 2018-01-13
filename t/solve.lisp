
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite solve :in :strips)
(in-suite solve)

(test movie1
  (with-parsed-information5 (-> (%rel "movie/p01.pddl")
                              parse
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (signals goal-found
      (eager #'goal-count))))
