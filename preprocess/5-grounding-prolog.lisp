
#|

An old attempt which does not use first-order predicates in the variables.
Heavily relies on arities and not really good.

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

(defun all-terms ()
  `((:- (all-terms (/ ?head ?arity) ?list)
        (functor ?term ?head ?arity)
        (findall ?term ?term ?list))
    (all-terms (list) (list))
    (:- (all-terms (list* ?first ?rest) ?list)
        (all-terms ?first ?list2)
        (all-terms ?rest ?list3)
        (append ?list2 ?list3 ?list))))

(defun reachable-fact-arities ()
  (let (arities)
    (iter (for proposition in *init*)
          (pushnew (length proposition) arities))
    (iter (for a in *actions*)
          (ematch a
            ((plist :effect effects)
             (dolist (e effects)
               (match e
                 (`(forall ,_ (when ,_ (not ,_)))       nil)
                 (`(forall ,_ (when ,_ (increase ,@_))) nil)
                 (`(forall ,_ (when ,_ ,atom))
                   (pushnew (length atom) arities)))))))
    (iter (for a in *axioms*)
          (ematch a
            ((list :derived predicate _)
             (pushnew (length predicate) arities))))
    arities))

(defun unreachable-fact-arities ()
  (let (arities)
    (iter (for a in *actions*)
          (ematch a
            ((plist :effect effects)
             (dolist (e effects)
               (match e
                 (`(forall ,_ (when ,_ (not ,atom)))
                   (pushnew (length atom) arities)))))))
    arities))

(defun reachable-op-arities ()
  (remove-duplicates (mapcar (lambda (a) (length (getf a :parameters))) *actions*)))

(defun relaxed-reachability ()
  (let ((arities (reachable-fact-arities))
        (un-arities (unreachable-fact-arities))
        (op-arities (reachable-op-arities)))
    (append
     (iter (for len in arities)    (collecting `(:- (table (/ reachable-fact ,len)))))
     (iter (for len in un-arities) (collecting `(:- (table (/ unreachable-fact ,len)))))
     (iter (for len in op-arities) (collecting `(:- (table (/ reachable-op ,(1+ len))))))
     (sort-clauses
      (append
       (iter (for proposition in *init*)
             (collecting `(initially-true ,@proposition)))
       (iter (for len in arities)
             (for args = (make-gensym-list len "?"))
             (collecting
              `(:- (reachable-fact ,@args)
                   (initially-true ,@args))))
       (iter (for len in un-arities)
             (for args = (make-gensym-list len "?"))
             (collecting
              `(:- (unreachable-fact ,@args)
                   (not (initially-true ,@args)))))
       (iter (for a in *actions*)
             (ematch a
               ((plist :action name
                       :parameters params
                       :precondition `(and ,@precond)
                       :effect effects)
                (collecting
                 `(:- (reachable-op ,name ,@params)
                      ,@(iter (for pre in precond)
                              (match pre
                                ((list 'not pre)
                                 (collecting `(unreachable-fact ,@pre)))
                                (_
                                 (collecting `(reachable-fact ,@pre)))))))
                (dolist (e effects)
                  (match e
                    (`(forall ,_ (when ,_ (increase ,@_))) nil)
                    (`(forall ,_ (when (and ,@conditions) (not ,atom)))
                      (collecting
                       `(:- (unreachable-fact ,@atom)
                            ,@(iter (for c in conditions)
                                    (match c
                                      ((list 'not c)
                                       (collecting `(unreachable-fact ,@c)))
                                      (_
                                       (collecting `(reachable-fact ,@c)))))
                            (reachable-op ,name ,@params))))
                    (`(forall ,_ (when (and ,@conditions) ,atom))
                      (collecting
                       `(:- (reachable-fact ,@atom)
                            ,@(iter (for c in conditions)
                                    (match c
                                      ((list 'not c)
                                       (collecting `(unreachable-fact ,@c)))
                                      (_
                                       (collecting `(reachable-fact ,@c)))))
                            (reachable-op ,name ,@params)))))))))
       (iter (for a in *axioms*)
             (ematch a
               ((list :derived predicate `(and ,@body))
                (collecting
                 `(:- (reachable-fact ,@predicate)
                      ,@(iter (for c in body)
                              (match c
                                ((list 'not c)
                                 (collecting `(unreachable-fact ,@c)))
                                (_
                                 (collecting `(reachable-fact ,@c))))))))))))
     ;; output facts/ops
     `((:- relaxed-reachability
           (write ":facts\\n")
           (all-terms (list ,@(iter (for len in arities) (collecting `(/ reachable-fact ,len)))) ?list)
           (print-sexp ?list)
           (write ":ops\\n")
           (all-terms (list ,@(iter (for len in op-arities) (collecting `(/ reachable-op ,(1+ len))))) ?list2)
           (print-sexp ?list2))))))

(defun fluent-fact-arities ()
  (let (arities)
    (iter (for a in *actions*)
          (ematch a
            ((plist :effect effects)
             (dolist (e effects)
               (match e
                 (`(forall ,_ (when ,_ (increase ,@_)))
                   nil)
                 (`(forall ,_ (when ,_ (not ,atom)))
                   (pushnew (length atom) arities))
                 (`(forall ,_ (when ,_ ,atom))
                   (pushnew (length atom) arities)))))))
    (iter (for a in *axioms*)
          (ematch a
            ((list :derived predicate _)
             (pushnew (length predicate) arities))))
    arities))

(defun fluent-facts ()
  (let ((arities (fluent-fact-arities)))
    (append (iter (for len in (reachable-fact-arities))
                  (for args = (make-gensym-list len "?"))
                  (appending
                   (if (member len arities)
                       `((:- (table (/ fluent-fact ,len)))
                         (:- (table (/ static-fact ,len)))
                         (:- (static-fact ,@args)
                             (reachable-fact ,@args)
                             (not (fluent-fact ,@args))))
                       
                       `((:- (table (/ static-fact ,len)))
                         (:- (static-fact ,@args)
                             (reachable-fact ,@args))))))
            (sort-clauses
             (append
              (iter (for a in *actions*)
                    (ematch a
                      ((plist :action name
                              :parameters params
                              :effect effects)
                       (dolist (e effects)
                         (match e
                           (`(forall ,_ (when ,_ (increase ,@_)))
                             nil)
                           (`(forall ,_ (when (and ,@conditions) (not ,atom)))
                             (collecting
                              `(:- (fluent-fact ,@atom)
                                   ,@(iter (for c in conditions)
                                           (collect `(reachable-fact ,@c)))
                                   (reachable-op ,name ,@params))))
                           (`(forall ,_ (when (and ,@conditions) ,atom))
                             (collecting
                              `(:- (fluent-fact ,@atom)
                                   ,@(iter (for c in conditions)
                                           (collect `(reachable-fact ,@c)))
                                   (reachable-op ,name ,@params)))))))))
              (iter (for a in *axioms*)
                    (ematch a
                      ((list :derived predicate `(and ,@body))
                       (collecting
                        `(:- (fluent-fact ,@predicate)
                             (reachable-fact ,@predicate)
                             (or ,@(iter (for c in body)
                                         (when (member (length c) arities) ; otherwise it is not a fluent
                                           (collecting
                                            `(fluent-fact ,@c))))))))))))
            `((:- fluent-facts
                  (write ":fluents\\n")
                  (all-terms (list ,@(iter (for len in arities)
                                           (collecting
                                            `(/ fluent-fact ,len))))
                             ?list)
                  (print-sexp ?list)
                  (write "\\n")
                  (write ":static\\n")
                  (all-terms (list ,@(iter (for len in (reachable-fact-arities))
                                           (collecting
                                            `(/ static-fact ,len))))
                             ?list2)
                  (print-sexp ?list2)
                  (write "\\n"))))))

(defun axiom-layer-arities ()
  (remove-duplicates (mapcar (compose #'length #'second) *axioms*)))

(defun iota-program ()
  `((:- (iota 0 (list))
        !)
    (:- (iota ?n ?list)
        (< ?n 0)
        !
        fail)
    (:- (iota ?n (list* ?n1 ?list2))
        (> ?n 0)
        !
        (is ?n1 (- ?n 1))
        (iota ?n1 ?list2))))

(defun axiom-layers ()
  (let ((axiom-arities (axiom-layer-arities))
        (fluent-arities (fluent-fact-arities)))
    (append
     (iota-program)
     (iter (for len in (union axiom-arities fluent-arities))
           (collecting
            `(:- (table (/ axiom-layer ,(1+ len))))))
     (sort-clauses
      (append
       (iter (for len in (union axiom-arities fluent-arities))
             (for args = (make-gensym-list len "?"))
             (collecting
              `(:- (axiom-layer ?n ,@args)
                   (< ?n 0)
                   !
                   fail)))
       (iter (for len in fluent-arities)
             (for args = (make-gensym-list len "?"))
             (collecting
              `(:- (axiom-layer 0 ,@args)
                   !
                   (fluent-fact ,@args))))
       (iter (for a in *axioms*)
             (ematch a
               ((list :derived predicate `(and ,@body))
                (collecting
                 `(:- (axiom-layer ?n ,@predicate)
                      (> ?n 0)
                      !
                      (is ?n1 (- ?n 1))
                      (not (axiom-layer ?n1 ,@predicate))
                      (iota ?n ?list)
                      ,@(iter (for c in body)
                              (for i from 0)
                              (for ?n = (symbolicate '?n (princ-to-string i)))
                              (appending
                               `((or (and (static-fact ,@c))
                                     (and (member ,?n ?list)
                                          (axiom-layer ,?n ,@c)))))))))))))
     (iter (for len in axiom-arities)
           (for args = (make-gensym-list len "?"))
           (collecting
            `(:- (axiom-layers-aux ?n)
                 nl
                 (findall (list ,@args) (and (axiom-layer ?n ,@args)
                                             (print-sexp (axiom-layer ?n ,@args)))
                          ?result)
                 (or (-> (\= ?result (list))
                       (and (is ?n1 (+ ?n 1))
                            (axiom-layers-aux ?n1)))
                     true))))
     `((:- axiom-layers
           (write ":axiom-layer (")
           (axiom-layers-aux 0)
           (write ")\\n"))))))

(defun %ground ()
  (run-prolog
   (append `((:- (use_module (library tabling))) ; swi specific
             (:- (style_check -singleton)))
           (relaxed-reachability)
           (fluent-facts)
           (axiom-layers)
           (print-sexp)
           (all-terms)
           `((:- main
                 (write "(") 
                 relaxed-reachability
                 fluent-facts
                 axiom-layers
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
