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
  (:use :cl :iterate :alexandria :trivia :pddl :arrow-macros :cl-prolog2)
  (:shadowing-import-from :trivia :<>)
  (:export
   #:parse
   #:variablep
   #:free
   #:with-parsed-information
   #:*types*
   #:*objects*
   #:*predicates*
   #:*init*
   #:*goal*
   #:*axioms*
   #:*actions*
   #:%rel
   #:*predicate-types*
   #:ground
   #:*monotonicity*
   #:with-parsed-information2
   #:easy-invariant
   #:*enable-no-op-pruning*
   #:*enable-negative-precondition-pruning-for-fluents*
   #:*enable-negative-precondition-pruning-for-axioms*
   #:*enable-negative-precondition-pruning*))

(setf trivia:*arity-check-by-test-call* nil)
