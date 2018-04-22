
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite solve :in :strips)
(in-suite solve)

(defun solve-alien-common (path fn)
  (declare (optimize (debug 3) (speed 0)))
  (log:info "Testing ~a" path)
  (recompile-instance-dependent-code)
  (sb-ext:gc :full t)
  (let* ((path (%rel path)))
    (strips::with-temp (planfile :debug t)
      (solve-once-to-file (find-domain path) path planfile fn)
      planfile)))

(defun solve-alien-blind (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (blind)))))))))

(defun solve-alien-gc (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (goal-count)))))))))

(defun solve-alien-ff/rpg (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (ff/rpg)))))))))

(defun solve-alien-ff/rpg-cached (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (cached-bucket-open-list
                              (ff/rpg)))))))))

(defun solve-alien-ff/rpg-lazy (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (lazy-bucket-open-list
                              (ff/rpg)))))))))

(defun solve-alien-novelty1 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty1)))))))))

(defun solve-alien-novelty2 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty2)))))))))

(defun solve-alien-novelty3 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty3)))))))))

(defun solve-alien-novelty4 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty4)))))))))

(defun solve-alien-novelty3-zdd (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty :k 3)))))))))

(defun solve-alien-novelty4-zdd (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty :k 4)))))))))

(defun solve-alien-novelty5-zdd (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (novelty :k 5)))))))))

(defun solve-alien-bwfs2 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (tiebreak
                               (novelty2)
                               (ff/rpg))))))))))

(defun solve-alien-bwfs3 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (tiebreak
                               (novelty3)
                               (ff/rpg))))))))))

(defun solve-alien-wffw11 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty1)
          h
          (local-novelty1 h)))))))))))

(defun solve-alien-wffw22 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty2)
          h
          (local-novelty2 h)))))))))))

(defun solve-alien-wffw12 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty1)
          h
          (local-novelty2 h)))))))))))

(defun solve-alien-wffw21 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty2)
          h
          (local-novelty1 h)))))))))))

(defun solve-alien-alien (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (alien/rpg)))))))))

(defun solve-alien-alien-lazy (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (lazy-bucket-open-list
                              (alien/rpg)))))))))

(defun solve-alien-alien2 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (tiebreak
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alien2-cached (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (cached-bucket-open-list
                              (tiebreak
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alien2-lazy (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (lazy-bucket-open-list
                              (tiebreak
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alien3 (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (tiebreak
                               (novelty2)
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alien3-lazy (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (lazy-bucket-open-list
                              (tiebreak
                               (novelty2)
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alien3-cached (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (cached-bucket-open-list
                              (tiebreak
                               (novelty2)
                               (alien/rpg)
                               (ff/rpg))))))))))

(defun solve-alien-alieni (path)
  (solve-alien-common path
                      (lambda ()
                        (with-memory-usage-diff ()
                          (strips:run
                           (timeout
                            *time-limit*
                            (ocl
                             (bucket-open-list
                              (tiebreak
                               (ff/rpg)
                               (alien/rpg))))))))))

(defun solve-fd-common (path option)
  (declare (optimize (debug 3) (speed 0)))
  (log:info "Testing ~a" path)
  (let* ((path (%rel path))
         (strips::*start-time* (get-internal-real-time))
         (strips::*last-milestone* strips::*start-time*))
    (unwind-protect
         (strips::with-temp (planfile :debug t)
           (uiop:run-program (list (namestring (strips::fd-relative-pathname "fast-downward.py"))
                                   "--run-all"
                                   "--overall-memory-limit" (princ-to-string *memory-limit*)
                                   "--translate-time-limit"  (princ-to-string *time-limit*)
                                   "--preprocess-time-limit" (princ-to-string *time-limit*)
                                   "--search-time-limit"     (princ-to-string *time-limit*)
                                   "--plan-file" (namestring planfile)
                                   (namestring (find-domain path))
                                   (namestring path)
                                   "--translate-options" "--invariant-generation-max-candidates" "0"
                                   "--search-options" "--search" option)
                             :output t
                             :ignore-error-status t)
           planfile)
      (strips::log-milestone :fd))))

(defun solve-fd-blind (path)
  (solve-fd-common path "eager(single_buckets(blind(),queue_type=LIFO),cost_type=ONE)"))

(defun solve-fd-ff (path)
  (solve-fd-common path "eager(single_buckets(ff(cost_type=ONE),queue_type=LIFO),cost_type=ONE)"))

(defvar *solver* #'solve-alien-blind)
(defun solve (path &optional (fn *solver*))
  (handler-case
      (let ((planfile (funcall fn path))
            (path (%rel path)))
        (is-true (validate-plan (strips:find-domain path)
                                path
                                planfile)))
    ((or sb-ext:timeout error) (c)
      ;; for 
      (fail "While ~a in ~a:~%caused ~a:~% Reason: ~a" fn path (type-of c) c)
      (skip "No plan found, no validation performed"))))

(test instance-depdenent
  (finishes
    (print
     (ocl
      (bucket-open-list
       (blind))))))

(test movie-basics
  (solve-alien-common
   "movie/p01.pddl"
   (lambda ()
     (strips:run
      (strips::make-searcher
       :storage nil
       :form `(lambda ()
                (print
                 (decode-state #*11111111))
                (let ((s (make-state+axioms)))
                  (replace s #*11111110)
                  (is (equal #*11111111
                             (apply-axioms s))))
                (is-true (strips::goalp #*11111111))
                (is-true (strips::goalp #*00000001))
                (signals goal-found
                  (report-if-goal #*11111111 (lambda ())))
                (signals goal-found
                  (report-if-goal #*00000001 (lambda ())))
                (is (equal #*11111110
                           (strips::goals)))
                (is (equal #*11111110
                           (strips::non-axiom-goals)))))))))

(test movie
  (iter (for *solver* in (list #'solve-alien-blind
                               #'solve-alien-gc
                               #'solve-alien-ff/rpg
                               #'solve-alien-ff/rpg-cached
                               #'solve-alien-ff/rpg-lazy
                               #'solve-alien-novelty1
                               #'solve-alien-novelty2
                               #'solve-alien-novelty3
                               #'solve-alien-novelty4
                               #'solve-alien-bwfs2
                               #'solve-alien-bwfs3
                               #'solve-alien-alien
                               #'solve-alien-alien2
                               #'solve-alien-alien3
                               #'solve-alien-alien3-cached
                               #'solve-alien-alien3-lazy))
        (solve "movie/p01.pddl")
        (solve "movie/p10.pddl")
        (solve "movie/p20.pddl")))

(test floortile
  (iter (for *solver* in (list #'solve-alien-blind
                               #'solve-alien-gc
                               #'solve-alien-ff/rpg
                               #'solve-alien-ff/rpg-lazy
                               #'solve-alien-novelty1
                               #'solve-alien-novelty2
                               #'solve-alien-novelty3
                               #'solve-alien-novelty4
                               #'solve-alien-bwfs2
                               #'solve-alien-bwfs3
                               #'solve-alien-alien
                               #'solve-alien-alien-lazy
                               #'solve-alien-alien2
                               #'solve-alien-alien2-lazy
                               #'solve-alien-alien3
                               #'solve-alien-alien3-lazy))
        (solve "ipc2014-agl/floortile-agl14/p01.pddl")))

(test demo
  ;; demo problems for IPC submission
  (solve "demo/sokoban/p01.pddl")
  (solve "demo/cavediving/p01.pddl")
  (solve "demo/citycar/p01.pddl")
  (solve "demo/parkprinter/p01.pddl")
  (solve "demo/researchers/p01.pddl"))

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
  (solve "ipc2011-opt/woodworking-opt11/p01.pddl"))

(test demo-agl14
  (solve "ipc2014-agl/barman-agl14/p01.pddl")
  (solve "ipc2014-agl/cavediving-agl14/p01.pddl")
  (solve "ipc2014-agl/childsnack-agl14/p01.pddl")
  (solve "ipc2014-agl/citycar-agl14/p01.pddl")
  (solve "ipc2014-agl/floortile-agl14/p01.pddl")
  (solve "ipc2014-agl/ged-agl14/p01.pddl")
  (solve "ipc2014-agl/hiking-agl14/p01.pddl")
  (solve "ipc2014-agl/maintenance-agl14/p01.pddl")
  (solve "ipc2014-agl/openstacks-agl14/p01.pddl")
  (solve "ipc2014-agl/parking-agl14/p01.pddl")
  (solve "ipc2014-agl/tetris-agl14/p01.pddl")
  (solve "ipc2014-agl/thoughtful-agl14/p01.pddl")
  (solve "ipc2014-agl/transport-agl14/p01.pddl")
  (solve "ipc2014-agl/visitall-agl14/p01.pddl"))

(test movie-ff/rpg
  (solve-alien-common
   "movie/p01.pddl"
   (lambda ()
     (ff/rpg)
     (strips:run
      (strips::make-searcher
       :storage nil
       :form `(lambda ()
                (is (= 7 (uiop:symbol-call :strips :ff-heuristic/rpg #*00000000)))))))))

(test demo-ff/rpg (let ((*solver* #'solve-alien-ff/rpg)) (run! 'demo)))
(test demo-fd-ff  (let ((*solver* #'solve-fd-ff))        (run! 'demo)))
(test demo-large-ff/rpg (let ((*solver* #'solve-alien-ff/rpg)) (run! 'demo-large)))
(test demo-large-fd-ff  (let ((*solver* #'solve-fd-ff))        (run! 'demo-large)))

(test demo-novelty1 (let ((*solver* #'solve-alien-novelty1)) (run! 'demo)))
(test demo-novelty2 (let ((*solver* #'solve-alien-novelty2)) (run! 'demo)))
(test demo-novelty3 (let ((*solver* #'solve-alien-novelty3)) (run! 'demo)))
(test demo-novelty4 (let ((*solver* #'solve-alien-novelty4)) (run! 'demo)))
(test demo-novelty3-zdd (let ((*solver* #'solve-alien-novelty3-zdd)) (run! 'demo)))
(test demo-novelty4-zdd (let ((*solver* #'solve-alien-novelty4-zdd)) (run! 'demo)))
(test demo-novelty5-zdd (let ((*solver* #'solve-alien-novelty5-zdd)) (run! 'demo)))

(test demo-bwfs2 (let ((*solver* #'solve-alien-bwfs2)) (run! 'demo)))
(test demo-bwfs3 (let ((*solver* #'solve-alien-bwfs3)) (run! 'demo)))
(test demo-alien (let ((*solver* #'solve-alien-alien)) (run! 'demo)))
(test movie-alien (let ((*solver* #'solve-alien-alien)) (run! 'movie)))
(test demo-large-alien (let ((*solver* #'solve-alien-alien)) (run! 'demo-large)))

(test movie-alien2 (let ((*solver* #'solve-alien-alien2)) (run! 'movie)))
(test demo-alien2 (let ((*solver* #'solve-alien-alien2)) (run! 'demo)))
(test demo-large-alien2 (let ((*solver* #'solve-alien-alien2)) (run! 'demo-large)))

(test movie-alien3 (let ((*solver* #'solve-alien-alien3)) (run! 'movie)))
(test demo-alien3 (let ((*solver* #'solve-alien-alien3)) (run! 'demo)))
(test demo-large-alien3 (let ((*solver* #'solve-alien-alien3)) (run! 'demo-large)))

(test demo-alieni (let ((*solver* #'solve-alien-alieni)) (run! 'demo)))

(test demo-large-novelty1 (let ((*solver* #'solve-alien-novelty1)) (run! 'demo-large)))
(test demo-large-novelty2 (let ((*solver* #'solve-alien-novelty2)) (run! 'demo-large)))
(test demo-large-novelty3 (let ((*solver* #'solve-alien-novelty3)) (run! 'demo-large)))
(test demo-large-novelty3-zdd (let ((*solver* #'solve-alien-novelty3-zdd)) (run! 'demo-large)))
(test demo-large-novelty4-zdd (let ((*solver* #'solve-alien-novelty4-zdd)) (run! 'demo-large)))
(test demo-large-novelty5-zdd (let ((*solver* #'solve-alien-novelty5-zdd)) (run! 'demo-large)))

(test sg-limit
  (let ((*time-limit* 3)
        (*compile-verbose* nil)
        (*compile-print* nil))
    (iter (for p in
               '("ipc2014-agl/barman-agl14/p20.pddl"
                 "ipc2014-agl/cavediving-agl14/p20.pddl"
                 "ipc2014-agl/childsnack-agl14/p20.pddl"
                 "ipc2014-agl/citycar-agl14/p20.pddl"
                 "ipc2014-agl/floortile-agl14/p20.pddl"
                 "ipc2014-agl/ged-agl14/p20.pddl"
                 "ipc2014-agl/hiking-agl14/p20.pddl"
                 "ipc2014-agl/maintenance-agl14/p20.pddl"
                 "ipc2014-agl/openstacks-agl14/p20.pddl"
                 "ipc2014-agl/parking-agl14/p20.pddl"
                 "ipc2014-agl/tetris-agl14/p20.pddl"
                 "ipc2014-agl/thoughtful-agl14/p20.pddl"
                 "ipc2014-agl/transport-agl14/p20.pddl"
                 "ipc2014-agl/visitall-agl14/p20.pddl"))
          (iter (for *sg-compiled-branch-limit*
                     from 0 to 3000 by 1000)
                (solve p)))))

(test demo-agl14-alien (let ((*solver* #'solve-alien-alien)) (run! 'demo-agl14)))
(test demo-agl14-alien2 (let ((*solver* #'solve-alien-alien2)) (run! 'demo-agl14)))
(test demo-agl14-alien3 (let ((*solver* #'solve-alien-alien3)) (run! 'demo-agl14)))

(test demo-ff/rpg-cached (let ((*solver* #'solve-alien-ff/rpg-cached)) (run! 'demo)))
(test demo-ff/rpg-lazy (let ((*solver* #'solve-alien-ff/rpg-lazy)) (run! 'demo)))
(test demo-alien-lazy (let ((*solver* #'solve-alien-alien-lazy)) (run! 'demo)))
(test demo-alien2-cached (let ((*solver* #'solve-alien-alien2-cached)) (run! 'demo)))
(test demo-alien2-lazy (let ((*solver* #'solve-alien-alien2-lazy)) (run! 'demo)))
(test demo-alien3-cached (let ((*solver* #'solve-alien-alien3-cached)) (run! 'demo)))
(test demo-alien3-lazy (let ((*solver* #'solve-alien-alien3-lazy)) (run! 'demo)))

(test demo-alien-wffw11 (let ((*solver* #'solve-alien-wffw11)) (run! 'demo)))
(test demo-alien-wffw22 (let ((*solver* #'solve-alien-wffw22)) (run! 'demo)))
(test demo-alien-wffw12 (let ((*solver* #'solve-alien-wffw12)) (run! 'demo)))
(test demo-alien-wffw21 (let ((*solver* #'solve-alien-wffw21)) (run! 'demo)))
