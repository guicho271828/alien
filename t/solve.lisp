
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite solve :in :strips)
(in-suite solve)

(defun solve (path)
  (with-parsed-information5 (-> (%rel path)
                              parse
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (signals goal-found
      (eager #'blind))))

(test movie (solve "movie/p01.pddl"))
(test cavediving (solve "cavediving/p01.pddl"))
(test citycar (solve "citycar/p01.pddl"))
(test parkprinter (solve "parkprinter/p01.pddl"))
(test researchers (solve "researchers/p01.pddl"))
(test sokoban (solve "sokoban/p01.pddl"))
