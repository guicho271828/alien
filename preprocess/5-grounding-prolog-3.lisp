
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

;;; tools for reachability predicates

(defun rf (name)
  (symbolicate name '-f))
(defun ro (name)
  (symbolicate name '-o))
(defun re (name)
  (symbolicate name '-e))
(defun ra (name)
  (symbolicate name '-a))

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
  (if (tmp-p condition)
      condition
      (ematch condition
        ((list* name args)
         `(,(rf name) ,@args)))))

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
             (list* `(:- (table (/ ,tmp ,(length min-u))))
                    `(:- ,new
                         ;; min-c1 has larger arity; put min-c2 first
                         ,(tmp/relaxed-reachable min-c2)
                         ,(tmp/relaxed-reachable min-c1))
                    acc)))))))

;;; relaxed-reachability

(defun tabled (rules)
  (ematch (first rules)
    ((or `(:- (,name ,@(and params (not nil))) ,@_)
         `(,(and name (not :-)) ,@(and params (not nil))))
     (cons `(:- (table (/ ,name ,(length params))))
           (sort-clauses rules)))
    ((or `(:- (,_) ,@_)
         `(,(not :-)))
     ;; We implement a custom memoization because 0-ary tabling causes error
     (sort-clauses
      (iter (for r in rules)
            (collecting
             (ematch r
               (`(:- (,name) ,@body)
                 `(:- (,name) ,@body (asserta (:- (,name) !))))
               (`(,_)
                 r))))))))

(defun relaxed-reachability ()
  (append
   (iter (for (o . _) in *objects*)
         (collecting `(object ,o)))
   `((,(rf '=) ?o ?o))
   (let (rf ro temporary)
     (iter (for (name . params) in *init*)
           (push `(,(rf name) ,@params)
                 (getf rf (rf name))))
     (iter (for a in *actions*)
           (ematch a
             ((plist :action name
                     :parameters params
                     :precondition `(and ,@precond)
                     :effect `(and ,@effects))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 precond)
                (appendf temporary temporary-rules)
                (push
                 `(:- (,(ro name) ,@params)
                      ,@decomposed
                      ,@(iter (for p in params)
                              (collecting `(object ,p)))) ro))
              (dolist (e effects)
                (match e
                  (`(forall ,vars (when (and ,@conditions) ,atom))
                    (when (positive atom)
                      (multiple-value-bind (decomposed temporary-rules)
                          (all-relaxed-reachable2 conditions)
                        (appendf temporary temporary-rules)
                        (push
                         `(:- (,(rf (car atom)) ,@(cdr atom))
                              (,(ro name) ,@params)
                              ,@decomposed
                              ,@(iter (for p in vars)
                                      (collecting `(object ,p))))
                         (getf rf (rf (car atom))))))))))))
     (iter (for a in *axioms*)
           (ematch a
             ((list :derived predicate `(and ,@body))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 body)
                (appendf temporary temporary-rules)
                (push
                 `(:- (,(rf (car predicate)) ,@(cdr predicate))
                      ,@decomposed
                      ,@(iter (for p in (cdr predicate))
                              ;; parameters not referenced in the condition
                              (collecting `(object ,p))))
                 (getf rf (rf (car predicate))))))))
     (append
      (mappend (lambda (ro) (tabled (list ro))) ro)
      (mappend (lambda (rf) (tabled (nreverse (cdr rf)))) (plist-alist rf))
      temporary
      ;; output facts/ops
      `((:- relaxed-reachability
            (write ":facts\\n")
            (wrap
             (and ,@(iter (for (name . params) in *predicates*)
                          (when (and (not (eq name '=)) (getf rf (rf name)))
                            (collecting
                             `(forall (,(rf name) ,@params)
                                      (print-sexp (,name ,@params))))))))
            (write ":ops\\n")
            (wrap
             (and ,@(iter (for a in *actions*)
                          (ematch a
                            ((plist :action name
                                    :parameters params)
                             (collecting
                              `(forall (,(ro name) ,@params)
                                       (print-sexp (,name ,@params)))))))))))))))

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

