
#|

This is an attempt to write a grounder that operates on the lifted predicates.
Prolog does not try to search/backtrack the predicate names, unfortunately.

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defpattern polarity (condition polarity)
  "Pattern for matching against a single positive or negative condition.
When CONDITION is positive, then polarity is bound to 'POS.
When CONDITION is negative (i.e. of the form (NOT (PRED ARGS...))), then polarity is bound to 'NEG."
  (assert (symbolp polarity))
  `(or (and (list 'not ,condition) (<> ,polarity 'neg))
       (and ,condition  (<> ,polarity 'pos))))

(defun %condition-list (conditions)
  "Returns a prolog list for conditions"
  `(list ,@(remove-duplicates ; because the same type predicate may be added many times
            (iter (for c in conditions)
                  (ematch c
                    ((polarity c p)
                     (collecting
                      `(,p (,@c))))))
            :test 'equal)))

(defun setup-domain ()
  (append
   (iter (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :precondition `(and ,@precond)
                   :effect `(and ,@effects))
            (collecting
             `(action-spec
               (,name ,@params)
               ,(%condition-list precond)
               (list ,@(iter (for e in effects)
                             ;; in Prolog sense, parameters are universally quantified
                             (ematch e
                               (`(forall ,_ (when ,_ (increase ,@_))) nil)
                               (`(forall ,vars (when (and ,@conditions) ,(polarity atom p)))
                                 (collecting
                                  `(forall (list ,@vars)
                                           (when ,(%condition-list conditions)
                                             (,p (,@atom))))))))))))))
   (iter (for a in *axioms*)
         (ematch a
           ((list :derived `(,name ,@params) `(and ,@body))
            (collecting
             `(axiom-body
               (,name ,@params)
               ,(%condition-list body))))))
   (iter (for proposition in *init*)
         (collecting `(init ,proposition)))
   (iter (for pair in *objects*)
         (collecting `(object ,(car pair))))))

(defun relaxed-reachability ()
  (append
   `((:- (table (/ relaxed-reachable-fact 1)))
     (:- (table (/ relaxed-reachable-action 1)))
     (objects (list))
     (:- (objects (list* ?x ?rest))
         (object ?x)
         (objects ?rest))
     (relaxed-reachable-facts (list))
     (:- (relaxed-reachable-facts (list* (pos ?x) ?rest))
         (relaxed-reachable-fact ?x)
         (relaxed-reachable-facts ?rest))
     (:- (relaxed-reachable-facts (list* (neg ?) ?rest))
         (relaxed-reachable-facts ?rest))
     (:- (relaxed-reachable-action ?a)
         (action-spec ?a ?precond ?)
         (relaxed-reachable-facts ?precond)
         (compound_name_arguments ?a ? ?params)
         (objects ?params))
     (:- (relaxed-reachable-fact ?f)
         (init ?f))
     (:- (relaxed-reachable-fact ?e)
         (relaxed-reachable-action ?a)
         (action-spec ?a ? ?effects)
         (member (forall ?vars (when ?conditions (pos ?e))) ?effects)
         (relaxed-reachable-facts ?conditions)
         (objects ?vars))
     (:- (relaxed-reachable-fact ?f)
         (axiom-body ?f ?body)
         (compound_name_arguments ?f ? ?params)
         (objects ?params)
         (relaxed-reachable-facts ?body))
     ;; output facts/ops
     (:- relaxed-reachability
         (write ":facts\\n")
         (findall ?f (relaxed-reachable-fact ?f) ?list)
         (print-sexp ?list)
         (write ":ops\\n")
         (findall ?a (relaxed-reachable-action ?a) ?list2)
         (print-sexp ?list2)))))

(defun %ground ()
  (run-prolog
   (append `((:- (use_module (library tabling))) ; swi specific
             (:- (style_check (- singleton))))
           (setup-domain)
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

(with-parsed-information (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  (print (%ground)))



;; (print (-> "ipc2011-opt/transport-opt11/p01.pddl"
;;          (%rel)
;;          (parse)
;;          (ground)
;;          (instantiate-action-body)))
