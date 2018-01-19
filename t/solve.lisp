
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite solve :in :strips)
(in-suite solve)

(defun solve (path)
  (declare (optimize (debug 3) (speed 0)))
  (with-parsed-information5 (-> (%rel path)
                              parse
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (signals goal-found
      (eager #'blind))))

(test movie
  (with-parsed-information5 (-> (%rel "movie/p01.pddl")
                              parse
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (print
     (decode-state #*11111111))
    (let ((s (make-state)))
      (replace s #*11111110)
      (is (equal #*11111111
                 (apply-axioms s))))
    (signals goal-found
      (report-if-goal #*11111111))
    (signals goal-found
      (report-if-goal #*00000001)))
  
  ;; (solve "movie/p01.pddl")
  )

(test cavediving (solve "cavediving/p01.pddl"))
(test citycar (solve "citycar/p01.pddl"))
(test parkprinter (solve "parkprinter/p01.pddl"))
(test researchers (solve "researchers/p01.pddl"))
(test sokoban (solve "sokoban/p01.pddl"))
