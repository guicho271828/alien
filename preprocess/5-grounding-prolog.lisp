
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info)
  (with-parsed-information info
    (destructuring-bind (facts ops) (%ground)
      (list* :facts facts
             :ops ops
             info))))

(defun sort-clauses (rules)
  (sort rules #'< :key
        (lambda (rule)
          (ematch rule
            (`(:- ,target ,_) (length target))
            (_ (length rule))))))

(defun %ground ()
  (let ((*package* (find-package :pddl)))
    (read-from-string
     (run-prolog
      (append `((:- (set_prolog_flag warning off))
                (:- (set_prolog_flag contiguous_warning off)))
              (iter (for len in (remove-duplicates (mapcar #'length *predicates*)))
                    (collecting
                     `(:- (table (/ reachable-fact ,len)))))
              (iter (for len in (remove-duplicates (mapcar (lambda (a) (1+ (length (getf a :parameters))))
                                                           *actions*)))
                    (collecting
                     `(:- (table (/ reachable-op ,len)))))
              (sort-clauses
               (append
                (iter (for proposition in *init*)
                      (collect `(reachable-fact ,@proposition)))
                (iter (for a in *actions*)
                      (ematch a
                        ((plist :action name
                                :parameters params
                                :effect effects)
                         (dolist (e effects)
                           (match e
                             (`(forall ,_ (when (and ,@conditions) ,atom))
                               (unless (or (eq 'not (car atom))
                                           (eq 'increase (car atom)))
                                 ;; relaxed-reachable
                                 (collecting
                                  `(:- (reachable-fact ,@atom)
                                       (and ,@(iter (for c in conditions)
                                                    (collect `(reachable-fact ,@c)))
                                            (reachable-op ,name ,@params)))))))))))
                (iter (for a in *axioms*)
                      (ematch a
                        ((list :derived predicate `(and ,@body))
                         (collecting
                          `(:- (reachable-fact ,@predicate)
                               (and ,@(iter (for c in body)
                                            (collecting
                                             `(reachable-fact ,@c)))))))))))
              (sort-clauses
               (iter (for a in *actions*)
                     (ematch a
                       ((plist :action name
                               :parameters params
                               :precondition `(and ,@precond))
                        (collecting
                         `(:- (reachable-op ,name ,@params)
                              (and ,@(iter (for pre in precond)
                                           (collect `(reachable-fact ,@pre))))))))))
              `((:- main
                    (write "((")
                    fail))
              ;; output facts
              (iter (for predicate in *predicates*)
                    (collecting
                     `(:- main
                          (reachable-fact ,@predicate)
                          (write "(")
                          ,@(iter (for e in predicate)
                                  (unless (first-iteration-p)
                                    (collect `(write " ")))
                                  (collect `(write ,e)))
                          (write ")\\n")
                          fail)))
              `((:- main
                    (write ")\\n(")
                    fail))
              ;; output ops
              (iter (for a in *actions*)
                    (ematch a
                      ((plist :action name :parameters params)
                       (collecting
                        `(:- main
                             (reachable-op ,name ,@params)
                             (write "(")
                             (write ,name)
                             ,@(iter (for p in params)
                                     (collect `(write " "))
                                     (collect `(write ,p)))
                             (write ")\\n")
                             fail)))))
              `((:- main
                    (write "))")
                    halt)))
      :bprolog :args '("-g" "main")))))

(with-parsed-information (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (print (%ground)))
