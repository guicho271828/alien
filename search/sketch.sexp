-*- mode : lisp -*-

#|

Each function returns a storage specification for each state,
and the set of functions to inline.

scalar : 
+ evaluate(state) --- btw, g-value should have access to update it
+ value cache --- likely 32/64 bit integer

open list:
+ insert(list, stateid, key), select(list)
+ set of storages returned by each element in the key

eager:
+ init()
+ step()
+ state
  + FD uses SAS, which adds to the space requirement, so it used SegmentedVector.
    We already use binary representation (1 bit for 1 proposition)
+ status of the state (close, open)
+ parent
+ op (generator)
+ plus the storages used in open list

run:
+ collect the storage and calcurate the size of the structure
+ etc.

+ pack everything for the maximum locality
+ divide them into octets
+ unpack them

|#

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

