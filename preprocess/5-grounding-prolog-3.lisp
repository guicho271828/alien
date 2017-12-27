
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

;;; join ordering

(defun all-relaxed-reachable2 (conditions)
  (-<> conditions
    (remove-if-not #'positive arrow-macros:<>)
    (remove-duplicates :test 'equal)
    (join-ordering)))

(defun tmp-p (condition)
  (match condition
    ((list* (symbol :name (string* #\T #\M #\P _)) _)
     t)))

(defun tmp/relaxed-reachable (condition)
  `(,(if (tmp-p condition) 'temporary-reachable 'reachable-fact) ,condition))

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
    
(defun join-ordering (conditions)
  "Helmert09, p40. This impl takes O(N^2)"
  (iter (for c in conditions)
        (if (some #'variablep (cdr c))
            (collect c into lifted)
            (collect (tmp/relaxed-reachable c) into grounded))
        (finally
         (return
          (multiple-value-bind (decomposed temporary-rules)
              (join-ordering-aux lifted nil)
            (values (append grounded decomposed)
                    temporary-rules))))))

(defun join-ordering-aux (conditions acc)
  (if (<= (length conditions) 2)
      (values (mapcar #'tmp/relaxed-reachable conditions) acc)
      (multiple-value-bind (min-u min-c1 min-c2) (find-best-join conditions)
        (with-gensyms (tmp)
          (let ((new `(,tmp ,@min-u)))
            (join-ordering-aux
             (-<>> conditions
               (remove min-c1 arrow-macros:<> :test #'equal)
               (remove min-c2 arrow-macros:<> :test #'equal)
               (cons new))
             (cons `(:- (temporary-reachable ,new)
                        ;; min-c1 has larger arity; put min-c2 first
                        ,(tmp/relaxed-reachable min-c2)
                        ,(tmp/relaxed-reachable min-c1))
                   acc)))))))

;;; relaxed-reachability

(defun relaxed-reachability ()
  (append
   `((:- (table (/ reachable-fact 1)))
     (:- (table (/ reachable-op 1)))
     (:- (table (/ temporary-reachable 1))))
   (iter (for (o . _) in *objects*)
         (collecting `(object ,o)))
   (sort-clauses
    (append
     `((:- (reachable-fact ?f)
           (reachable-axiom ?f))
       (:- (reachable-fact ?f)
           (reachable-effect ?f))
       (reachable-fact (= ?o ?o)))
     (iter (for a in *actions*)
           (ematch a
             ((plist :action name
                     :parameters params
                     :precondition `(and ,@precond)
                     :effect `(and ,@effects))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 precond)
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
                      (multiple-value-bind (decomposed temporary-rules)
                          (all-relaxed-reachable2 conditions)
                        (appending temporary-rules)
                        (collecting
                         `(:- (reachable-effect ,atom)
                              (reachable-op (,name ,@params))
                              ,@decomposed
                              ,@(iter (for p in vars)
                                      (collecting `(object ,p)))))))))))))
     (iter (for a in *axioms*)
           (ematch a
             ((list :derived predicate `(and ,@body))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 body)
                (appending temporary-rules)
                (collecting
                 `(:- (reachable-axiom ,predicate)
                      ,@decomposed
                      ,@(iter (for p in (cdr predicate))
                              ;; parameters not referenced in the condition
                              (collecting `(object ,p)))))))))
     (iter (for i in *init*)
           (collect `(reachable-fact ,i)))))
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
         (print-sexp ?list3)))))

(defun %ground (&optional debug)
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
   :swi :args '("-g" "main") :debug debug))

