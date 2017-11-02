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
           :?o
           :exists
           :forall
           :imply
           ))

(defpackage strips
  (:use :cl :iterate :alexandria :trivia :pddl)
  (:export
   #:parse))

(setf trivia:*arity-check-by-test-call* nil)
