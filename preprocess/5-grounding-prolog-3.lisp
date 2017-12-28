
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

(defun positive (form)
  (match form
    ((list* (or 'not 'increase) _)
     nil)
    (_
     t)))

;;; grounding utility

(defun suffix (condition &rest suffixes)
  (match condition
    (`(,name ,@args)
      `(,(apply #'symbolicate name suffixes) ,@args))))

(defun reachable         (condition) (suffix condition '-rf))
(defun reachable-op      (condition) (suffix condition '-ro))
(defun reachable-effect  (condition i) (suffix condition '-re (princ-to-string i)))
(defun reachable-axiom   (condition) (suffix condition '-re))

(defun applicable-op     (condition) (suffix condition '-ao))
(defun applicable-effect (condition i) (suffix condition '-ae) (princ-to-string i))
(defun applicable-axiom  (condition) (suffix condition '-aa))

(defun apply-effect (condition i) (suffix condition '-ape) (princ-to-string i))
(defun apply-axiom  (condition) (suffix condition '-apa))

(defun triggered-op      (condition) (suffix condition '-to))
(defun triggered-effect  (condition i) (suffix condition '-te) (princ-to-string i))
(defun fact-triggered-axiom    (condition) (suffix condition '-fta))
(defun axiom-triggered-axiom   (condition) (suffix condition '-ata))

(defun new-fact          (condition) (suffix condition '-nf))
(defun new-axiom         (condition) (suffix condition '-na))

;;; relaxed-reachability

(defun relaxed-reachability (&aux (dummy (gensym)))
  (append
   (iter (for (o . _) in *objects*)
         ;; these are static, so they do not trigger anything
         (collecting `(object ,o)))

   `((reachable (= ?o ?o))
     ,@(iter (for p in *init*)
             (collecting
              `(reachable ,p))))
   
   `((:- (dynamic (/ new-fact 1)))
     (new-fact (= ?o ?o))
     (new-fact (,dummy))               ; dummy fact for triggering null-preconditioned action
     ;; initial state
     ,@(iter (for p in *init*)
             (collecting
              `(new-fact ,p))))
   
   `((:- (dynamic (/ new-axiom 1)))
     (new-axiom (= ?o ?o))
     (new-axiom (,dummy)))

   ;; trigger rules
   (iter (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :precondition `(and ,@precond))
            (collecting
             `(:- (triggered-op (,name ,@params))
                  (or ,@(if precond
                            (iter (for p in precond)
                                  (when (positive p)
                                    (collecting `(new-fact ,p))))
                            `((new-fact (,dummy))))))))))

   (iter outer
         (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :effect `(and ,@effects))
            (iter (for e in effects)
                  (for i from 0)
                  (match e
                    (`(forall ,_ (when (and ,@conditions) ,(satisfies positive)))
                      (when conditions
                        (in outer
                            (collecting
                             `(:- (triggered-effect (,name ,@params) ,i)
                                  (or ,@(iter (for p in conditions)
                                              (when (positive p)
                                                (collecting `(new-fact ,p)))))))))))))))

   (iter (for a in *axioms*)
         (ematch a
           ((list :derived predicate `(and ,@body))
            (collecting
             `(:- (fact-triggered-axiom ,predicate)
                  (or ,@(if body
                            (iter (for p in body)
                                  (when (positive p)
                                    (collecting `(new-fact ,p))))
                            `((new-fact (,dummy))))))))))

   (iter (for a in *axioms*)
         (ematch a
           ((list :derived predicate `(and ,@body))
            (when body
              (collecting
               `(:- (axiom-triggered-axiom ,predicate)
                    (or ,@(iter (for p in body)
                                (when (positive p)
                                  (collecting `(new-axiom ,p)))))))))))
   
   ;; applicability rules
   (iter (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :precondition `(and ,@precond))
            (collecting
             `(:- (applicable-op (,name ,@params))
                  (and ,@(iter (for p in precond)
                               (when (positive p)
                                 (collecting `(reachable ,p))))
                       ,@(iter (for p in params)
                               (collecting `(object ,p)))))))))

   (iter outer
         (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :effect `(and ,@effects))
            (iter (for e in effects)
                  (for i from 0)
                  (match e
                    (`(forall ,vars (when (and ,@conditions) ,(satisfies positive)))
                      (in outer
                          (collecting
                           `(:- (applicable-effect (,name ,@params) ,i)
                                (and (applicable-op (,name ,@params))
                                     ,@(iter (for p in conditions)
                                             (when (positive p)
                                               (collecting `(reachable ,p))))
                                     ,@(iter (for p in vars)
                                             (collecting `(object ,p)))))))))))))

   (iter (for a in *axioms*)
         (ematch a
           ((list :derived predicate `(and ,@body))
            (collecting
             `(:- (applicable-axiom ,predicate)
                  (and ,@(iter (for p in body)
                               (when (positive p)
                                 (collecting `(reachable ,p))))
                       ,@(iter (for p in (cdr predicate))
                               (collecting `(object ,p)))))))))

   ;; apply rules
   (iter outer
         (for a in *actions*)
         (ematch a
           ((plist :action name
                   :parameters params
                   :effect `(and ,@effects))
            (iter (for e in effects)
                  (for i from 0)
                  (match e
                    (`(forall ,_ (when ,_ ,(and atom (satisfies positive))))
                      (in outer
                          (collecting
                           `(:- (apply-effect (,name ,@params) ,i)
                                (assertz (reachable-op (,name ,@params)))
                                (assertz (reachable-effect (,name ,@params) i))
                                (assertz (reachable ,atom))
                                (assertz (new-fact ,atom)))))))))))

   (iter (for a in *axioms*)
         (ematch a
           ((list :derived predicate _)
            (collecting
             `(:- (apply-axiom ,predicate)
                  (and (assertz (new-axiom ,predicate))
                       (assertz (new-fact ,predicate))
                       (assertz (reachable ,predicate))))))))

   ;; exploration
   `((:- apply-axioms
         ;; no new-axiom at the moment
         (forall (and (fact-triggered-axiom ?op) (applicable-axiom ?op))
                 (apply-axiom ?op))
         ;; new-axiom added
         repeat
         (findall ?op (and (axiom-triggered-axiom ?op) (applicable-axiom ?op)) ?list)
         ;; axiom triggering was computed, previous new-axioms are no longer necessary
         (retractall (new-axiom ?x))
         ;; now no new-axiom at the moment
         (or (== (list) ?list)
             (and (forall (member ?op ?list)
                          (apply-axiom ?op))
                  ;; new-axiom added
                  fail)) ; goto repeat
         ;; new-facts:
         ;; new-facts in the previous action layer and
         ;; new-facts in this axiom layer
         )
     (:- apply-ops
         (findall (applicable-effect ?op ?i)
                  (and (or (triggered-op ?op)
                           (triggered-effect ?op ?i))
                       (applicable-op ?op)
                       (applicable-effect ?op ?i))
                  ?list)
         (retractall (new-fact ?x)) ; clear new-facts
         (or (== (list) ?list)
             (and (forall (member (applicable-effect ?op ?i) ?list)
                          (apply-effect ?op ?i))
                  fail))
         ;; new-facts produced in this layer
         )
     (:- expand
         repeat
         apply-axioms
         apply-ops))
   
   ;; output facts/ops
   `((:- relaxed-reachability
         expand
         (write ":facts\\n")
         (wrap
          (and ,@(iter (for p in *predicates*)
                       (when (not (eq (car p) '=))
                         (collecting
                          `(forall (reachable ,p) (print-sexp ,p)))))))
         (write ":ops\\n")
         (wrap
          (and ,@(iter (for a in *actions*)
                       (ematch a
                         ((plist :action name
                                 :parameters params)
                          (collecting
                           `(forall (reachable-op (,name ,@params))
                                    (print-sexp (,name ,@params)))))))))))))

(defun %ground (&optional debug)
  (run-prolog
   (append `((:- (use_module (library tabling))) ; swi specific
             (:- (style_check (- singleton))))
           (print-sexp :swi t)
           `((:- (wrap ?goal)
                 (write "(")
                 (call ?goal)
                 (write ")"))
             (:- main
                 (wrap relaxed-reachability)
                 halt))
           (relaxed-reachability))
   :swi :args '("-g" "main") :debug debug))

