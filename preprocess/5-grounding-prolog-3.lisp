
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

;;; join ordering

(defun all-relaxed-reachable2 (conditions)
  (-<> conditions
    (remove-if-not #'positive arrow-macros:<>)
    (remove-duplicates :test 'equal)
    (join-ordering)))

;;;; this join ordering implementation is too slow on large number of objects, e.g. visitall-agl14

(defun find-best-join (conditions)
  (let (min-u
        min-c1
        min-c2
        min-key1
        min-key2
        min-key3)
    (iter (for (c1 . rest) on (sort (copy-list conditions) #'> :key #'length))
          (for (_ . args1) = c1)
          (for vars1 = (remove-if-not #'variablep args1))
          (for l1 = (length vars1))
          (iter (for c2 in rest)
                (for (_ . args2) = c2)
                (for vars2 = (remove-if-not #'variablep args2))
                (for l2 = (length vars2))
                (for u  = (union vars1 vars2))
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
    (values min-u min-c1 min-c2)))
    
(defun tmp-p (condition)
  (match condition
    ((list* (symbol :name (string* #\T #\M #\P _)) _) t)))

(defun tmp/reachable (condition)
  (if (tmp-p condition)
      condition
      `(reachable ,condition)))

(defun join-ordering (conditions)
  "Helmert09, p40. This impl takes O(N^2)"
  (iter (for c in conditions)
        (if (some #'variablep (cdr c))
            (collect c into lifted)
            (collect (tmp/reachable c) into grounded))
        (finally
         (return
          (multiple-value-bind (decomposed temporary-rules)
              (join-ordering-aux lifted nil)
            (values (append grounded decomposed)
                    temporary-rules))))))

(defun join-ordering-aux (conditions acc)
  (if (<= (length conditions) 2)
      (values (mapcar #'tmp/reachable conditions) acc)
      (multiple-value-bind (min-u min-c1 min-c2) (find-best-join conditions)
        (with-gensyms (tmp)
          (let ((new `(,tmp ,@min-u)))
            (join-ordering-aux
             (-<>> conditions
               (remove min-c1 arrow-macros:<> :test #'equal)
               (remove min-c2 arrow-macros:<> :test #'equal)
               (cons new))
             (list* `(:- ,(tmp/reachable new)
                         ;; min-c1 has larger arity; put min-c2 first
                         ,(tmp/reachable min-c2)
                         ,(tmp/reachable min-c1))
                    acc)))))))

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
            (multiple-value-bind (decomposed temporary) (all-relaxed-reachable2 precond)
              (appending temporary into others)
              (collecting
               `(:- (applicable-op (,name ,@params))
                    (and ,@decomposed
                         ,@(iter (for p in params)
                                 (collecting `(object ,p)))))
               into main))))
         (finally (return (append main others))))

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
                      (multiple-value-bind (decomposed temporary) (all-relaxed-reachable2 conditions)
                        (in outer
                            (appending temporary into others)
                            (collecting
                             `(:- (applicable-effect (,name ,@params) ,i)
                                  (and (applicable-op (,name ,@params))
                                       ,@decomposed
                                       ,@(iter (for p in vars)
                                               (collecting `(object ,p)))))
                             into main))))))))
         (finally (return-from outer (append main others))))

   (iter (for a in *axioms*)
         (ematch a
           ((list :derived predicate `(and ,@body))
            (multiple-value-bind (decomposed temporary) (all-relaxed-reachable2 body)
              (appending temporary into others)
              (collecting
               `(:- (applicable-axiom ,predicate)
                    (and ,@decomposed
                         ,@(iter (for p in (cdr predicate))
                                 (collecting `(object ,p)))))
               into main))))
         (finally (return (append main others))))

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

