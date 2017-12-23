
#|

This is a rewrite of 5-grounding-prolog-3 which considers unreachable fact (facts that are reachable and
can be deleted).

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info)
  (with-parsed-information info
    (let ((result (%ground)))
      (destructuring-bind (&key facts ops fluents)
          (let ((*package* (find-package :pddl)))
            (read-from-string result))
        (format t "~%~a facts, ~a ops, ~a fluents." (length facts) (length ops) (length fluents))
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

(defun negative (form)
  (match form
    ((list* 'not _) t)))

(defun condition-satisfied (conditions)
  (iter (for c in conditions)
        (match c
          (`(increase ,@_) nil)
          (`(not ,c)
            ;; instantiate objects incrementally
            (dolist (p (cdr c))
              (collecting `(object ,p)))
            (collecting `(unreachable-fact ,c)))
          (_
           ;; instantiate objects incrementally
           (dolist (p (cdr c))
             (collecting `(object ,p)))
           (collecting `(reachable-fact ,c))))))

(defun relaxed-reachability ()
  (append
   `((:- (table (/ reachable-fact 1))) ; facts that can be true
     (:- (table (/ reachable-op 1)))   ; ops that can be applicable
     (:- (table (/ unreachable-fact 1))) ; facts that can be false. NOTE: not "the facts that cannot be true".
     (:- (table (/ changed-fact 2)))   ; facts added/deleted by an applicable action
     ;; NOTE: tabled predicates causes errors when there is no instance of the predicate.
     ;; However, it is unlikely that every action in a domain has no effects.
     (:- (reachable-fact ?f)
         (or (initially-true ?f)
             (changed-fact add ?f)))
     (:- (unreachable-fact ?f)
         (or (and (reachable-fact ?f)
                  (changed-fact del ?f))
             (not (reachable-fact ?f)))))
   #+(or)
   `((:- (table (/ fluent-fact 1)))
     (:- (table (/ static-true-fact 1)))
     (:- (table (/ static-false-fact 1)))
     (:- (fluent-fact ?f)
         ;; the ordering is critical, (and (not (init...)) (added...)) does not work
         (or (and (changed-fact add ?f)
                  (not (initially-true ?f)))
             (and (initially-true ?f)
                  (changed-fact del ?f))))
     (:- (static-true-fact ?f)
         (and (initially-true ?f)
              (not (changed-fact del ?f))))
     (:- (static-false-fact ?f)
         (and (not (initially-true ?f))
              (not (changed-fact add ?f)))))
   (iter (for (o . _) in *objects*)
         (collecting `(object ,o)))
   (iter (for c in *init*)
         (collecting `(initially-true ,c)))
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
                    ,@(condition-satisfied precond)))
              (dolist (e effects)
                (match e
                  (`(forall ,_ (when ,_ (increase ,@_))))
                  (`(forall ,_ (when (and ,@conditions) (not ,atom)))
                    (collecting
                     `(:- (changed-fact del ,atom)
                          ,@(condition-satisfied conditions)
                          (reachable-op (,name ,@params)))))
                  (`(forall ,_ (when (and ,@conditions) ,atom))
                    (collecting
                     `(:- (changed-fact add ,atom)
                          ,@(condition-satisfied conditions)
                          (reachable-op (,name ,@params))))))))))
     (iter (for a in *axioms*)
           (ematch a
             ((list :derived predicate `(and ,@body))
              (collecting
               `(:- (changed-fact add ,predicate)
                    ,@(condition-satisfied body)
                    ;; the ordering here is quite important; being an object is a bottom line, should be checked last
                    ,@(iter (for p in (cdr predicate)) (collecting `(object ,p))))))))))
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

(with-parsed-information (parse (%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  (print (%ground)))


;; when negative preconditions are ignored,
;; action A is always reachable.
;; otherwise, it is reachable only when (p ?x) is initially false and never added, or reachable but can be deleted.
;; since no action adds or deletes (p ?x), 

(defvar *sample-domain*
  '(define (domain d)
    (:requirements :strips :typing)
    (:predicates (p ?x) (goal))
    (:action A
     :parameters (?x)
     :precondition (not (p ?x))
     :effect (goal))))

(with-parsed-information (parse1 *sample-domain*
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 o2)
                                   (:init (p o1))
                                   (:goal (goal))))
  (print (%ground)))

(with-parsed-information (parse1 *sample-domain*
                                 '(define (problem p)
                                   (:domain d)
                                   (:objects o1 o2)
                                   (:init (goal))
                                   (:goal (goal))))
  (print (%ground)))
