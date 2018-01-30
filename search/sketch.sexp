-*- mode : lisp -*-





(solve-once domain problem plan-output-file
            (lambda ()
              (run
               (eager
                (bucket-open-list
                 (ff))))))

(solve-once domain problem plan-output-file
            (lambda ()
              (run
               (eager
                (tiebreaking-open-list
                 (ff)
                 (novelty))))))

(solve-once domain problem plan-output-file
            (lambda ()
              (run
               (eager
                (alternate-open-list
                 (tiebreaking-open-list
                  (ff))
                 (tiebreaking-open-list
                  (lmcount)))))))

(solve-once domain problem plan-output-file
            (lambda ()
              (run
               (eager
                (alternate-open-list
                 (let ((h (lmcut)))
                   (tiebreaking-open-list
                    (sum (g) h)
                    h)))
                :reopen t))))

(solve-once domain problem plan-output-file
            (lambda ()
              (run
               (portfolio
                (timelimit
                 300
                 (eager
                  (bucket-open-list
                   (ff-heuristics))))
                (timelimit
                 300
                 (eager
                  (bucket-open-list
                   (ff-heuristics))))))))

