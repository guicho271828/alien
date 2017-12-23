
#|

This is a rewrite of 5-grounding-prolog with minimally using the lifted predicates.

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info)
  (with-parsed-information info
    (let ((result (%ground)))
      (append (let ((*package* (find-package :pddl)))
                (read-from-string result))
              info))))

(progn ;; dummy functions for eldoc
  (defun precondition (?action ?condition-list)
    "dummy function for eldoc"
    nil)
  (defun effect (?action ?effect-list)
    "dummy function for eldoc"
    nil)
  (defun axiom-body (?head ?condition-list)
    "dummy function for eldoc"
    nil))

(defun positive (form)
  (match form
    ((list* (or 'not 'increase) _)
     nil)
    (_
     t)))

(defun all-relaxed-reachable (conditions)
  (iter (for c in (remove-duplicates conditions :test 'equal))
        (when (positive c)
          (collecting `(reachable-fact ,c)))))

(defparameter *use-join-ordering* nil)
(defparameter *use-join-ordering* t)

(defun all-relaxed-reachable2 (conditions)
  (if *use-join-ordering*
      (-<> conditions
        (remove-if-not #'positive arrow-macros:<>)
        (remove-duplicates :test 'equal)
        (join-ordering nil))
      (all-relaxed-reachable conditions)))

(defun tmp-p (condition)
  (match condition
    ((list* (symbol :name (string* #\T #\M #\P _)) _)
     t)))

(assert (tmp-p '(tmp111)))
(assert (not (tmp-p '(a))))

(defun tmp/relaxed-reachable (condition)
  `(,(if (tmp-p condition) 'temporary-reachable 'reachable-fact) ,condition))

(defun join-ordering (conditions acc)
  "Helmert09, p40"
  (if (<= (length conditions) 2)
      (values (mapcar #'tmp/relaxed-reachable conditions) acc)
      (let (min-u
            min-c1
            min-c2
            min-key1
            min-key2
            min-key3)
        (iter (for (c1 . rest) on (sort conditions #'> :key #'length))
              (for (_ . args1) = c1)
              (for l1 = (length args1))
              (iter (for c2 in rest)
                    (for (_ . args2) = c2)
                    (for l2 = (length args2))
                    (for u  = (union args1 args2))
                    (for key3 = (length u))
                    (for key1 = (- key3 l1))
                    (for key2 = (- key3 l2))
                    (when (or (null min-u)
                              (or (< key1 min-key1)
                                  (when (= key1 min-key1)
                                    (or (< key2 min-key2)
                                        (when (= key2 min-key2)
                                          (< key3 min-key3))))))
                      (setf min-u u
                            min-c1 c1
                            min-c2 c2
                            min-key1 key1
                            min-key2 key2
                            min-key3 key3))))
        (with-gensyms (tmp)
          (let ((new `(,tmp ,@min-u)))
            (join-ordering
             (-<>> conditions
               (remove min-c1 arrow-macros:<> :test #'equal)
               (remove min-c2 arrow-macros:<> :test #'equal)
               (cons new))
             (cons `(:- (temporary-reachable ,new)
                        ,(tmp/relaxed-reachable min-c1)
                        ,(tmp/relaxed-reachable min-c2))
                   acc)))))))

(print-values
 (all-relaxed-reachable2
  '((in-city ?l1 ?c)
    (in-city ?l2 ?c)
    (at ?t ?l1))))

(defmacro reverse-call (head &body args)
  (let ((syms (make-gensym-list (length args))))
    `(let ,(mapcar #'list syms (reverse args))
       (,head ,@(reverse syms)))))

(let ((a '()))
  (print
   (reverse-call append
                 a
                 (progn
                   (push 1 a)
                   '(2 3)))))


(defun relaxed-reachability ()
  (let ((tables
         `((:- (table (/ reachable-fact 1)))
           (:- (table (/ reachable-op 1)))
           (:- (table (/ temporary-reachable 1))))))
    (reverse-call append
      tables
      (iter (for (o . _) in *objects*)
            (collecting `(object ,o)))
      (sort-clauses
       (append
        (iter (for a in *actions*)
              (ematch a
                ((plist :action name
                        :parameters params
                        :precondition `(and ,@precond)
                        :effect `(and ,@effects))
                 (multiple-value-bind (decomposed temporary-rules)
                     (all-relaxed-reachable2 (shuffle (copy-list precond)))
                   (appending temporary-rules)
                   (collecting
                    `(:- (reachable-op (,name ,@params))
                         ,@decomposed
                         ,@(iter (for p in params)
                                 (collecting `(object ,p))))))
                 (dolist (e effects)
                   (match e
                     (`(forall ,vars (when (and ,@conditions) ,atom))
                       (when (positive atom)
                         (with-gensyms (reachable-effect)
                           (push `(:- (table (/ ,reachable-effect 1))) tables)
                           (appending
                            `((:- (reachable-fact ?f)
                                  (,reachable-effect ?f))
                              (:- (,reachable-effect ,atom)
                                  (reachable-op (,name ,@params))
                                  ,@(all-relaxed-reachable conditions)
                                  ,@(iter (for p in params)
                                          (collecting `(object ,p)))
                                  ,@(iter (for p in vars)
                                          (collecting `(object ,p))))))))))))))
        `((:- (reachable-fact ?f)
              (reachable-axiom ?f)))
        (iter (for a in *axioms*)
              (ematch a
                ((list :derived predicate `(and ,@body))
                 (multiple-value-bind (decomposed temporary-rules)
                     (all-relaxed-reachable2 (shuffle (copy-list body)))
                   (appending temporary-rules)
                   (collecting
                    `(:- (reachable-axiom ,predicate)
                         ,@decomposed
                         ,@(iter (for p in (cdr predicate))
                                 ;; parameters not referenced in the condition
                                 (collecting `(object ,p)))))))))
        (all-relaxed-reachable *init*)))
      ;; output facts/ops
      `((:- relaxed-reachability
            (write ":facts\\n")
            (findall ?f (reachable-fact ?f) ?list)
            (print-sexp ?list)
            (write ":ops\\n")
            (findall ?a (reachable-op ?a) ?list2)
            (print-sexp ?list2)
            (write ":axioms\\n")
            (findall ?f (reachable-axiom ?f) ?list3)
            (print-sexp ?list3))))))

(defun %ground ()
  (run-prolog
   (append `((:- (use_module (library tabling))) ; swi specific
             (:- (style_check (- singleton))))
           (relaxed-reachability)
           (print-sexp :swi t)
           `((:- main
                 (write "(") 
                 relaxed-reachability
                 (write ")")
                 halt)))
   :swi :args '("-g" "main") :debug t))

(with-parsed-information (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (print (%ground)))

(let ((*package* (find-package :pddl)))
  (print (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))))

(with-parsed-information (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  (print (%ground)))

(with-parsed-information (time (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl")))
  (time (%ground)))

;; (with-parsed-information (parse (%rel "axiom-domains/opttel-adl-derived/p30.pddl"))
;;   (time (%ground)))

(with-parsed-information (parse1 '(define (domain d)
                                   (:requirements :strips :typing)
                                   (:predicates (p ?x) (goal))
                                   (:action A
                                    :parameters (?x)
                                    :precondition (not (p ?x))
                                    :effect (goal)))
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 o2)
                                   (:init )
                                   (:goal (goal))))
  (print (%ground)))

(with-parsed-information (parse1 '(define (domain d)
                                   (:requirements :strips :typing)
                                   (:predicates (p ?x) (goal))
                                   (:action A
                                    :parameters (?x)
                                    :precondition (p ?x)
                                    :effect (goal)))
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 o2)
                                   (:init )
                                   (:goal (goal))))
  (print (%ground)))

(with-parsed-information (parse1 '(define (domain d)
                                   (:requirements :strips :typing)
                                   (:predicates (p ?x) (goal))
                                   (:action A
                                    :parameters (?x)
                                    :precondition (p ?x)
                                    :effect (goal)))
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 o2)
                                   (:init (p o1))
                                   (:goal (goal))))
  (print (%ground)))

(with-parsed-information (parse1 '(define (domain d)
                                   (:requirements :strips :typing)
                                   (:types a b)
                                   (:predicates (p ?x) (goal))
                                   (:action A
                                    :parameters (?x - a)
                                    :precondition (p ?x)
                                    :effect (goal)))
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 - a o2 - b)
                                   (:init (p o1) (p o2))
                                   (:goal (goal))))
  (print (%ground)))

(with-parsed-information (parse1 '(define (domain d)
                                   (:requirements :strips :typing)
                                   (:predicates (p1) (p2) (goal))
                                   (:action A
                                    :parameters ()
                                    :precondition (p1)
                                    :effect (p2))
                                   (:action B
                                    :parameters ()
                                    :precondition (p2)
                                    :effect (goal)))
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects)
                                   (:init (p1))
                                   (:goal (goal))))
  (print (%ground)))

;; axiom

(defun call-test-ground (info fn)
  (with-parsed-information info
    (let ((result (%ground)))
      ;; (print result)
      (apply fn (read-from-string result)))))

(defmacro with-test-ground (info &body body)
  `(call-test-ground ,info
                     (lambda (&key facts ops fluents axioms)
                       (declare (ignorable facts ops fluents axioms))
                       ,@body)))

(defun mem (elem list)
  (member elem list :test 'equal))

;; parameter ?x is not referenced in the precondition
(with-test-ground (parse1 '(define (domain d)
                            (:requirements :strips :typing)
                            (:predicates (d ?x) (p ?x) (goal))
                            (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                            (:derived (d ?x) (p ?x)))
                          '(define (problem p)
                            (:domain d)
                            (:objects o1 o2)
                            (:init )
                            (:goal (goal))))
  (assert (mem '(d o1) facts))
  (assert (mem '(p o1) facts))
  (assert (mem '(d o2) facts))
  (assert (mem '(p o2) facts))
  (assert (mem '(a o1) ops))
  (assert (mem '(a o2) ops)))

;; parameter ?x is not referenced in the axiom body
(with-test-ground (parse1 '(define (domain d)
                            (:requirements :strips :typing)
                            (:predicates (d ?x) (p) (goal))
                            (:action a :parameters (?x) :precondition (and) :effect (p))
                            (:derived (d ?x) (p)))
                          '(define (problem p)
                            (:domain d)
                            (:objects o1 o2)
                            (:init )
                            (:goal (goal))))
  (assert (mem '(p) facts))
  (assert (mem '(d o1) facts))
  (assert (mem '(d o2) facts))
  (assert (mem '(a o1) ops))
  (assert (mem '(a o2) ops)))

;; parameter ?x is a free variable in the axiom body
(with-test-ground (parse1 '(define (domain d)
                            (:requirements :strips :typing)
                            (:predicates (d ?x) (p ?x) (goal))
                            (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                            (:derived (d) (p ?x)))
                          '(define (problem p)
                            (:domain d)
                            (:objects o1 o2)
                            (:init )
                            (:goal (goal))))
  (assert (mem '(p o1) facts))
  (assert (mem '(p o2) facts))
  (assert (mem '(d) facts))
  (assert (= 1 (count '(d) facts :test 'equal)))
  (assert (mem '(a o1) ops))
  (assert (mem '(a o2) ops)))

(with-test-ground (parse1 '(define (domain d)
                            (:requirements :strips :typing)
                            (:predicates (d ?x) (p ?x) (p2 ?x) (goal))
                            (:action a :parameters (?x) :precondition (and) :effect (p ?x))
                            (:derived (d) (p2 ?x)))
                          '(define (problem p)
                            (:domain d)
                            (:objects o1 o2)
                            (:init )
                            (:goal (goal))))
  (assert (mem '(p o1) facts))
  (assert (mem '(p o2) facts))
  (assert (not (mem '(p2 o1) facts)))
  (assert (not (mem '(p2 o2) facts)))
  (assert (not (mem '(d) facts)))
  (assert (mem '(a o1) ops))
  (assert (mem '(a o2) ops)))

(with-test-ground (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  (assert (= 286 (length ops))))

(with-test-ground (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (assert (= 616 (length ops))))
