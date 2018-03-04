#|
  This file is a part of strips project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)

(defpackage pddl
  (:import-from :cl :and :or :not :when :-
                := :> :< :>= :<= :/ :* :+
                :nil)
  (:export :define
           :domain
           :object
           :problem
           :increase
           :total-cost
           :?o :?o1 :?o2
           :exists
           :forall
           :imply
           :either)
  (:documentation "The package for loading the symbols in the input"))

(defpackage strips
  (:use :cl :iterate :alexandria :trivia :pddl :arrow-macros :cl-prolog2
        :cl-cudd)
  (:shadowing-import-from :trivia :<>)
  (:shadow :iterate :else)
  (:shadow :info :node) ; cl-cudd
  (:import-from :strips.lib
                :packed-aref
                :size-of)
  (:export
   ;; translate
   #:parse
   #:variablep
   #:free
   #:*types*
   #:*objects*
   #:*predicates*
   #:*init*
   #:*goal*
   #:*axioms*
   #:*actions*
   #:%rel
   #:*predicate-types*
   #:with-parsed-information

   ;; preprocessing
   #:easy-invariant
   #:*monotonicity*
   #:with-parsed-information2

   ;; grounding
   #:*enable-no-op-pruning*
   #:*enable-negative-precondition-pruning-for-fluents*
   #:*enable-negative-precondition-pruning-for-axioms*
   #:*enable-negative-precondition-pruning*
   #:ground
   #:*grounding-prolog*
   #:*ground-axioms*
   #:*facts*
   #:*ops*
   #:with-parsed-information3
   
   #:axiom-layers
   #:mutex-invariant
   #:*axiom-layers*
   #:*axiom-layer-prolog*
   #:with-parsed-information4
   
   #:instantiate
   #:index-facts
   #:instantiate-ops
   #:*fact-index*
   #:*fact-size*
   #:*fact-trie*
   #:*instantiated-ops*
   #:*op-index*
   #:*sg*
   #:with-parsed-information5
   #:*instantiated-axiom-layers*
   #:*state-size*

   ;; misc utils
   #:memory-usage
   #:with-memory-usage-diff
   #:print-values
   #:println
   #:find-domain
   #:max-state-id

   ;; search external
   #:goal-found
   #:no-solution
   #:close-list-full
   #:solve-once
   #:solve-once-to-file
   #:run
   #:*memory-limit*
   #:*time-limit*
   
   ;; search internal
   #:report-if-goal
   #:applicable-ops
   #:apply-axioms
   #:apply-op
   #:make-state

   ;; plan reporting
   #:retrieve-path
   #:output-plan
   #:validate-plan
   
   ;; plan reporting internal
   #:decode-op
   #:decode-state

   ;; search algorithm/wrapper
   #:eager
   #:timeout
   
   ;; open list
   #:bucket-open-list
   
   ;; heuristics
   #:goal-count
   #:blind
   #:ff/rpg
   #:make-state+axioms
   #:recompile-instance-dependent-code
   #:do-leaf
   #:bell
   #:*sg-compiled-branch-limit*
   #:*effect-compilation-threashold*
   #:solve-common))

(setf trivia:*arity-check-by-test-call* nil)
