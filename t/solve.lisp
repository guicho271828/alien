
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite solve :in :strips)
(in-suite solve)

(defun solve (path)
  (declare (optimize (debug 3) (speed 0)))
  (log:info "Testing ~a" path)
  (recompile-instance-dependent-code)
  (let* ((path (%rel path))
         plan)
    (handler-case
        (progn
          (setf plan
                (solve-once (find-domain path) path
                            (lambda ()
                              (strips:run
                               (eager
                                (bucket-open-list
                                 (goal-count)))))))
          (pass "plan found"))
      (error (c)
        (fail "in ~a:~%caused ~a:~% Reason: ~a" path (type-of c) c)))
    (if plan
        (is-true (validate-plan (strips:find-domain path)
                                path
                                plan))
        (skip "No plan found, no validation performed"))))

(defun solve-fd (path)
  (declare (optimize (debug 3) (speed 0)))
  (log:info "Testing ~a" path)
  (let* ((path (%rel path)))
    (strips::with-temp (planfile :debug t)
      (uiop:run-program (list (namestring (strips::fd-relative-pathname "fast-downward.py"))
                              "--run-all"
                              "--plan-file" (namestring planfile)
                              (namestring (find-domain path))
                              (namestring path)
                              "--search" "eager(single_buckets(goalcount()))")
                        :output t))))

(test movie-basics
  (with-parsed-information5 (-> (%rel "movie/p01.pddl")
                              parse
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (print
     (decode-state #*11111111))
    (let ((s (make-state+axioms)))
      (replace s #*11111110)
      (is (equal #*11111111
                 (apply-axioms s))))
    (signals goal-found
      (report-if-goal #*11111111 (lambda ())))
    (signals goal-found
      (report-if-goal #*00000001 (lambda ())))))

(test movie
  (solve "movie/p01.pddl")
  (solve "movie/p10.pddl")
  (solve "movie/p20.pddl"))

(test demo
  (solve "demo/sokoban/p01.pddl")
  (solve "demo/cavediving/p01.pddl")
  ;; ;; (solve "demo/citycar/p01.pddl") ; too difficult for goal-count
  (solve "demo/parkprinter/p00.pddl")
  (solve "demo/parkprinter/p01.pddl")
  (solve "demo/researchers/p01.pddl")
  (solve "demo/researchers-debug/p01.pddl"))

(test demo-large
  ;; VAL doesnt work
  ;; (solve "axiom-domains/opttel-adl-derived/p01.pddl")
  ;; (solve "axiom-domains/opttel-strips-derived/p01.pddl")
  ;; (solve "axiom-domains/philosophers-adl-derived/p01.pddl")
  ;; (solve "axiom-domains/philosophers-strips-derived/p01.pddl")
  ;; (solve "axiom-domains/psr-middle-adl-derived/p01.pddl")
  ;; (solve "axiom-domains/psr-middle-strips-derived/p01.pddl")
  
  ;; (solve "check/negative-precondition-test/p01.pddl")
  ;; (solve "check/rovers-noop/p01.pddl")
  ;; (solve "check/type-test/p01.pddl")

  ;; (solve "downward/benchmarks/openstacks/p01.pddl")
  ;; (solve "downward/benchmarks/openstacks-opt08-adl/p01.pddl")
  ;; (solve "downward/benchmarks/openstacks-strips/domain_p01.pddl")
  ;; (solve "downward/benchmarks/openstacks-strips/p01.pddl")
  ;; (solve "downward/benchmarks/pathways/p01.pddl")
  ;; (solve "downward/benchmarks/pathways-noneg/p01.pddl")
  ;; (solve "downward/benchmarks/rovers/p01.pddl")
  ;; (solve "downward/benchmarks/storage/p01.pddl")
  ;; (solve "downward/benchmarks/tpp/p01.pddl")
  ;; (solve "downward/benchmarks/trucks/p01.pddl")
  ;; (solve "downward/benchmarks/trucks-strips/domain_p01.pddl")
  ;; (solve "downward/benchmarks/trucks-strips/p01.pddl")

  (solve "ipc2006-optsat/openstacks/p01.pddl")
  (solve "ipc2006-optsat/pathways/p01.pddl")
  (solve "ipc2006-optsat/pipesworld/p01.pddl")
  (solve "ipc2006-optsat/rovers/p01.pddl")
  ;; (solve "ipc2006-optsat/storage/p01.pddl") ; EITHER type
  (solve "ipc2006-optsat/tpp/p01.pddl")
  (solve "ipc2006-optsat/trucks/p01.pddl")
  (solve "ipc2008-opt/elevators-opt08/p01.pddl")
  (solve "ipc2008-opt/openstacks-opt08/p01.pddl")
  (solve "ipc2008-opt/parcprinter-opt08/p01.pddl")
  (solve "ipc2008-opt/pegsol-opt08/p01.pddl")
  (solve "ipc2008-opt/scanalyzer-opt08/p01.pddl")
  (solve "ipc2008-opt/sokoban-opt08/p01.pddl")
  (solve "ipc2008-opt/transport-opt08/p01.pddl")
  (solve "ipc2008-opt/woodworking-opt08/p01.pddl")
  (solve "ipc2011-opt/barman-opt11/p01.pddl")
  (solve "ipc2011-opt/elevators-opt11/p01.pddl")
  (solve "ipc2011-opt/floortile-opt11/p01.pddl")
  (solve "ipc2011-opt/nomystery-opt11/p01.pddl")
  (solve "ipc2011-opt/openstacks-opt11/p01.pddl")
  (solve "ipc2011-opt/parcprinter-opt11/p01.pddl")
  (solve "ipc2011-opt/parking-opt11/p01.pddl")
  (solve "ipc2011-opt/pegsol-opt11/p01.pddl")
  (solve "ipc2011-opt/scanalyzer-opt11/p01.pddl")
  (solve "ipc2011-opt/sokoban-opt11/p01.pddl")
  (solve "ipc2011-opt/tidybot-opt11/p01.pddl")
  (solve "ipc2011-opt/transport-opt11/p01.pddl")
  (solve "ipc2011-opt/visitall-opt11/p01.pddl")
  (solve "ipc2011-opt/woodworking-opt11/p01.pddl")
  ;; (solve "ipc2014-agl/barman-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/cavediving-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/childsnack-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/citycar-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/floortile-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/ged-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/hiking-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/maintenance-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/openstacks-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/parking-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/tetris-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/thoughtful-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/transport-agl14/p01.pddl")
  ;; (solve "ipc2014-agl/visitall-agl14/p01.pddl")
  )


(test instance-depdenent
  (finishes
    (print
     (eager
      (bucket-open-list
       (blind))))))
