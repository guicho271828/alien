
#|

This is a rewrite of 5-grounding-prolog with minimally using the lifted predicates.

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info)
  (with-parsed-information info
    (let ((result (%ground)))
      (destructuring-bind (&key facts ops fluents)
          (let ((*package* (find-package :pddl)))
            (read-from-string result))
        (list* :facts facts
               :ops ops
               :fluents fluents
               info)))))

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
  (iter (for c in conditions)
        (when (positive c)
          (collecting `(reachable-fact ,c)))))

(defun relaxed-reachability ()
  (append
   `((:- (table (/ reachable-fact 1)))
     (:- (table (/ reachable-op 1))))
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
              (collecting
               `(:- (reachable-op (,name ,@params))
                    ,@(all-relaxed-reachable precond)
                    ;; the ordering here is quite important; being an object is a bottom line, should be checked last
                    ,@(iter (for p in params) (collecting `(object ,p)))))
              (dolist (e effects)
                (match e
                  (`(forall ,_ (when (and ,@conditions) ,atom))
                    (when (positive atom)
                      (collecting
                       `(:- (reachable-fact ,atom)
                            ,@(all-relaxed-reachable conditions)
                            (reachable-op (,name ,@params)))))))))))
     (iter (for a in *axioms*)
           (ematch a
             ((list :derived predicate `(and ,@body))
              (collecting
               `(:- (reachable-fact ,predicate)
                    ,@(all-relaxed-reachable body)
                    ;; the ordering here is quite important; being an object is a bottom line, should be checked last
                    ,@(iter (for p in (cdr predicate)) (collecting `(object ,p))))))))
     (all-relaxed-reachable *init*)))
   ;; output facts/ops
   `((:- relaxed-reachability
         (write ":facts\\n")
         (findall ?f (reachable-fact ?f) ?list)
         (print-sexp ?list)
         (write ":ops\\n")
         (findall ?a (reachable-op ?a) ?list2)
         (print-sexp ?list2)))))

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

