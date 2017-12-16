
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(print (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl")))

(with-parsed-information (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (print *objects*)
  (print *objects*)
  )

(defun ground (info)
  (with-parsed-information info
    (multiple-value-bind (propositions transitions) (%ground)
      (list* :propositions propositions
             :transitions transitions
             info))))

(defun %ground ()
  (run-prolog
   (append `((:- (use_module (library tabling)))
             ,@(iter (for predicate in *predicates*)
                     (collecting
                      `(:- table fib/2))))
           (iter (for proposition in *init*)
                 (collect `(,@proposition)))
           (iter (for a in *actions*)
                 (ematch a
                   ((plist :action name
                           :parameters params
                           :precondition `(and ,@precond)
                           :effect effects)
                    (collecting
                     `(:- (,name ,@params)
                          (and ,@(iter (for pre in precond)
                                       (collect `(,@pre))))))
                    (dolist (e effects)
                      (match e
                        (`(forall ,_ (when (and ,@conditions) ,atom))
                          (unless (or (eq 'not (car atom))
                                      (eq 'increase (car atom)))
                            ;; relaxed-reachable
                            (collecting
                             `(:- (,@atom)
                                  (and ,@(iter (for c in conditions)
                                               (collect `(,@c)))
                                       (,name ,@params)))))))))))
           `((:- main
                 (write "aaa\\n")))
           (iter (for predicate in *predicates*)
                 (collecting
                  `(:- main
                       (,@predicate)
                       (write "(")
                       ,@(iter (for e in predicate)
                               (unless (first-iteration-p)
                                 (collect `(write " ")))
                               (collect `(write ,e)))
                       (write ")\\n")
                       fail)))
           `((:- (initialization main))))
   :swi :debug t))

(with-parsed-information (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (print *actions*)
  (print (%ground)))

;; (send-rules p (iter (for p in *predicates*)
;;                     (collecting
;;                      `(:- (pprint-facts ,(first p))
;;                           
;; 
;; (send-rules p `(:- dump-all-atoms
;;                    (and (setof ?p (solve ?p) ?set))
;; 
;; 
;; (send-query p `(and (factorial 3 ?w)
;;                     (write ?w)
;;                     (write "\\n")
;;                     fail)
;;             (lambda (output)
;;               (iter (with start = (get-universal-time))
;;                     (for now = (get-universal-time))
;;                     (when (> (- now start) 3)
;;                       (error "timeout!"))
;;                     (until (listen output)))
;;               (is (eql #\6 (peek-char nil output)))
;;               (is (= 6 (read output)))
;;               nil))))
