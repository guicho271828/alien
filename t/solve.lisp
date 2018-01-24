
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
    ;; (print-values
    ;;   (with-timing
    ;;     (signals goal-found
    ;;       (eager #'blind))))
    ;; (print-values
    ;;   (with-timing
    (finishes
      (block nil
        (handler-bind ((goal-found
                        (lambda (c)
                          (declare (ignore c))
                          (print (retrieve-path))
                          (return))))
          (eager #'goal-count))))))

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
  
  (solve "movie/p01.pddl")
  (solve "movie/p02.pddl")
  (solve "movie/p03.pddl")
  (solve "movie/p04.pddl")
  (solve "movie/p05.pddl")
  (solve "movie/p06.pddl")
  (solve "movie/p07.pddl")
  (solve "movie/p08.pddl")
  (solve "movie/p09.pddl")
  (solve "movie/p10.pddl")
  (solve "movie/p11.pddl")
  (solve "movie/p12.pddl")
  (solve "movie/p13.pddl")
  (solve "movie/p14.pddl")
  (solve "movie/p15.pddl")
  (solve "movie/p16.pddl")
  (solve "movie/p17.pddl")
  (solve "movie/p18.pddl")
  (solve "movie/p19.pddl")
  (solve "movie/p20.pddl"))

(test demo
  (solve "demo/sokoban/p01.pddl")
  (solve "demo/cavediving/p01.pddl")
  ;; (solve "demo/citycar/p01.pddl") ; error in sg
  ;; (solve "demo/parkprinter/p01.pddl") ; list exhausted
  ;; (solve "demo/researchers/p01.pddl") ; error in sg
  )
