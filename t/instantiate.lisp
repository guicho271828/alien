
(in-package :strips.test)

(named-readtables:in-readtable :fare-quasiquote)

(def-suite instantiate :in :strips)
(in-suite instantiate)

(defun call-test-instantiate (info fn)
  (with-parsed-information5 (-> info
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (funcall fn)))

(defmacro with-test-instantiate (info &body body)
  `(call-test-instantiate ,info (lambda () ,@body)))

(defun println (x)
  (princ x) (terpri))

(test instantiate1
  (with-test-instantiate (strips::parse1 'pddl::(define (domain d)
                                           (:requirements :strips :typing)
                                           (:predicates (d ?x) (p ?x) (goal))
                                           (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                                           (:derived (d ?x) (p ?x)))
                                         'pddl::(define (problem p)
                                           (:domain d)
                                           (:objects o1 o2)
                                           (:init )
                                           (:goal (goal))))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    (finishes (println *instantiated-ops*))))

(test instantiate2
  ;; parameter ?x is not referenced in the axiom body
  (with-test-instantiate (strips::parse1 'pddl::(define (domain d)
                                           (:requirements :strips :typing)
                                           (:predicates (d ?x) (p) (goal))
                                           (:action a :parameters (?x) :precondition (and) :effect (p))
                                           (:derived (d ?x) (p)))
                                         'pddl::(define (problem p)
                                           (:domain d)
                                           (:objects o1 o2)
                                           (:init )
                                           (:goal (goal))))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    (finishes (println *instantiated-ops*))))

(test instantiate3
  ;; parameter ?x is a free variable in the axiom body
  (with-test-instantiate (strips::parse1 'pddl::(define (domain d)
                                           (:requirements :strips :typing)
                                           (:predicates (d) (p ?x) (goal))
                                           (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                                           (:derived (d) (p ?x)))
                                         'pddl::(define (problem p)
                                           (:domain d)
                                           (:objects o1 o2)
                                           (:init )
                                           (:goal (goal))))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    (finishes (println *instantiated-ops*))))

(test instantiate4
  (with-test-instantiate (strips::parse1 'pddl::(define (domain d)
                                           (:requirements :strips :typing)
                                           (:predicates (d ?x) (p ?x) (p2 ?x) (goal))
                                           (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                                           (:derived (d) (p2 ?x)))
                                         'pddl::(define (problem p)
                                           (:domain d)
                                           (:objects o1 o2)
                                           (:init )
                                           (:goal (goal))))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    (finishes (println *instantiated-ops*))))

(test instantiate5
  (with-test-instantiate (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    ;; (finishes (println *instantiated-ops*))
    ))

(test instantiate6
  (with-test-instantiate (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
    (finishes (println *index*))
    (finishes (println *trie*))
    (finishes (println *fluent-size*))
    ;; (finishes (println *instantiated-ops*))
    ))

(test instantiate7 ; initially true vs false predicates which are never deleted
  (let ((*enable-negative-precondition-pruning-for-fluents* t))
    (with-test-instantiate (strips::parse1
                            'pddl::(define (domain d)
                              (:requirements :strips :typing)
                              (:predicates (p ?x) (q ?x))
                              (:action a :parameters (?x) :precondition (and (not (p ?x))) :effect (q ?x)))
                            'pddl::(define (problem p)
                              (:domain d)
                              (:objects a b)
                              (:init (p a))
                              (:goal (and))))
      (finishes (println *index*))
      (finishes (println *trie*))
      (finishes (println *fluent-size*))
      (finishes (println *instantiated-ops*)))))

(test instantiate8 ; initially true predicates which can be deleted vs which is never deleted
  (let ((*enable-negative-precondition-pruning-for-fluents* t))
    (with-test-instantiate (strips::parse1
                            'pddl::(define (domain d)
                              (:requirements :strips :typing)
                              (:predicates (p ?x) (q ?x) (r ?x))
                              (:action a :parameters (?x) :precondition (not (p ?x)) :effect (q ?x))
                              (:action a :parameters (?x) :precondition (r ?x)       :effect (not (p ?x))))
                            'pddl::(define (problem p)
                              (:domain d)
                              (:objects a b)
                              (:init (p a) (p b) (r b))
                              (:goal (and))))
      (finishes (println *index*))
      (finishes (println *trie*))
      (finishes (println *fluent-size*))
      (finishes (println *instantiated-ops*)))))

(test instantiate9 ; axioms that can become true vs cannot become true
  (let ((*enable-negative-precondition-pruning-for-axioms* t)
        (*enable-negative-precondition-pruning-for-fluents* t))
    (with-test-instantiate (strips::parse1
                            'pddl::(define (domain d)
                              (:requirements :strips :typing)
                              (:predicates (p ?x) (q ?x) (axiom ?x))
                              (:action a :parameters (?x) :precondition (and (not (axiom ?x))) :effect (q ?x))
                              (:derived (axiom ?x) (p ?x)))
                            ;; if (p ?x) can be negative, then (axiom ?x) can also be negative
                            'pddl::(define (problem p)
                              (:domain d)
                              (:objects a b)
                              (:init (p a))
                              (:goal (and))))
      (finishes (println *index*))
      (finishes (println *trie*))
      (finishes (println *fluent-size*))
      (finishes (println *instantiated-ops*)))))

(test instantiate10 ; axioms that can become true vs cannot become true
  (let ((*enable-negative-precondition-pruning-for-axioms* t)
        (*enable-negative-precondition-pruning-for-fluents* t))
    (with-test-instantiate (strips::parse1
                            'pddl::(define (domain d)
                              (:requirements :strips :typing)
                              (:predicates (p ?x) (q ?x) (axiom ?x) (r ?x))
                              (:action a :parameters (?x) :precondition (and (not (axiom ?x))) :effect (q ?x))
                              (:action a :parameters (?x) :precondition (r ?x)                 :effect (not (p ?x)))
                              (:derived (axiom ?x) (p ?x)))
                            'pddl::(define (problem p)
                              (:domain d)
                              (:objects a b)
                              (:init (p a) (p b) (r b))
                              (:goal (and))))
      (finishes (println *index*))
      (finishes (println *trie*))
      (finishes (println *fluent-size*))
      (finishes (println *instantiated-ops*)))))


