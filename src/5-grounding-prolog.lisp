
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

(defparameter *interpreter-class* 'cl-prolog.swi:swi-prolog)

(defmacro with-impl ((var) &body body)
  `(let ((,var (make-instance *interpreter-class*)))
     (unwind-protect
          (progn ,@body)
       (cl-prolog:terminate ,var))))

(defun %ground ()
  (with-impl (p)
    (send-rules p (iter (for proposition in *init*)
                        (collect `(relaxed-reachable-fact ,@proposition))))
    (send-rules p (iter (for a in *actions*)
                        (ematch a
                          ((plist :action name
                                  :parameters params
                                  :precondition `(and ,@precond)
                                  :effect effects)
                           (collecting
                            `(:- (relaxed-reachable-op ,name ,@params)
                                 (and ,@(iter (for pre in precond)
                                              (collect `(relaxed-reachable-fact ,@pre))))))
                           (dolist (e effects)
                             (match e
                               (`(forall ,_ (when (and ,conditions) ,atom))
                                 (unless (or (eq 'not (car atom))
                                             (eq 'increase (car atom)))
                                   ;; relaxed-reachable
                                   (collecting
                                    `(:- (relaxed-reachable-fact ,@atom)
                                         (and ,@(iter (for c in conditions)
                                                      (collect `(relaxed-reachable-fact ,@c)))
                                              (relaxed-reachable-op ,name ,@params))))))))))))
    (iter (for predicate in *predicates*)
          (send-query p `(relaxed-reachable-fact ,@predicate)
                      (lambda (output)
                        (print (read-line output)))))))

(with-parsed-information (parse (%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (%ground))

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
