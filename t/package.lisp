#|
  This file is a part of strips project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :strips.test
  (:use :cl
        :strips :pddl
        :fiveam
        :iterate :alexandria :trivia))
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)


(def-suite :strips)
(in-suite :strips)

;; run test with (run! test-name) 

(defvar *problems*
  (iter (for p in (directory (asdf:system-relative-pathname :strips #p"*/*/*.pddl")))
        (match p
          ((pathname :name (guard name (not (search "domain" name :from-end t))))
           (collecting p result-type vector)))))

(defun test-condition (c objects predicates)
  (ematch c
    ((or `(not (,name ,@args))
         `(,name ,@args))
     (is (every #'atom args))
     (is (not (member '- args)))
     (dolist (a args)
       (is-true (or (variablep a)
                    (member a objects :key #'car))
                "~a is not a variable nor an object in ~a" a objects))
     (is (not (member name `(and or imply forall exists))))
     (let ((found (assoc name predicates)))
       (is-true found "predicate = ~a not found in predicates: ~%~a" name predicates)
       (is (= (1+ (length args)) (length found)))))
    (_ (let ((*package* (find-package :pddl)))
         (fail "Condition does not follow the expected form: ~%~s" c)))))

(test strips
  (finishes
    (let ((*package* (find-package :cl-user)))
      (parse (asdf:system-relative-pathname :strips "axiom-domains/opttel-adl-derived/p01.pddl"))))

  (for-all ((p (lambda () (random-elt *problems*))))
    (let (parsed)
      (finishes
        (setf parsed (parse p)))
      (match parsed
        ((list :type types
               :objects objects
               :predicates predicates
               :init init
               :goal goal
               :axioms axioms
               :actions actions)
         (iter (for (type . super) in types)
               (is (atom type))
               (is (atom super)))
         (iter (for (name . type) in objects)
               (is (atom type))
               (is (atom name)))
         (is-false (member '- objects))
         (dolist (p predicates)
           (is-false (member '- p)))
         (let ((list (flatten goal)))
           (is-false (member 'or list))
           (is-false (member 'imply list))
           (is-false (member 'exists list))
           (is-false (member 'forall list))
           (is-false (member '- list)))
         (let ((list (flatten axioms)))
           (is-false (member 'or list))
           (is-false (member 'imply list))
           (is-false (member 'exists list))
           (is-false (member 'forall list))
           (is-false (member '- list)))
         (dolist (a actions)
           (match a
             (`(:action ,name
                        :parameters ,p
                        :original-parameters ,op
                        :precondition (and ,@conditions)
                        :effect (and ,@effects))
               (is-true (every #'variablep p))
               (is-true (every #'variablep op))
               (dolist (c conditions)
                 (test-condition c objects predicates))
               (dolist (e effects)
                 (ematch e
                   (`(forall ,args (when (and ,@conditions) (increase (total-cost) ,_)))
                     (dolist (c conditions)
                       (test-condition c objects predicates))
                     (is-true (every #'variablep args))
                     (is-false (member '- args)))
                   (`(forall ,args (when (and ,@conditions) ,c))
                     (dolist (c conditions)
                       (test-condition c objects predicates))
                     (is-true (every #'variablep args))
                     (is-false (member '- args))
                     (test-condition c objects predicates))
                   (_ (let ((*package* (find-package :pddl)))
                        (fail "Effect does not follow the expected form: ~%~s" a))))))
             (_ (let ((*package* (find-package :pddl)))
                  (fail "Action does not follow the expected form: ~%~s" a))))))))))
