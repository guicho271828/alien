
#|

This is a rewrite of 5-grounding-prolog with minimally using the lifted predicates.

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info)
  (with-parsed-information2 info
    (let ((result (%ground)))
      (append (let ((*package* (find-package :pddl)))
                (read-from-string result))
              info))))

;;; tools for reachability predicates

(defun tmp-p (condition)
  (match condition
    ((list* (symbol :name (string* #\T #\M #\P _)) _)
     t)))

;; to circumvent the problem related to zero-ary predicates and memoization.
;; tabling zero-ary predicates is not possible

(defun normalize-fact-term (term)
  (if (tmp-p term)
      term
      (ematch term
        ((list name)
         `(fact ,name))
        ((list* name args)
         `(,(symbolicate name '-f) ,@args)))))

(defun normalize-fact-rule (rule)
  "Normalize the head of the fact rule. Body should be normalized separately"
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-fact-term head) ,@body))
    (_
     (normalize-fact-term rule))))

(defun normalize-op-term (term)
  (if (tmp-p term)
      term
      (ematch term
        ((list name)
         `(op ,name))
        ((list* name args)
         `(,(symbolicate name '-o) ,@args)))))

(defun normalize-op-rule (rule)
  "Normalize the head of the op rule. Body should be normalized separately"
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-op-term head) ,@body))
    (_
     (normalize-op-term rule))))

;; heads are normalzied in the last step

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
    
(defun join-ordering (conditions)
  "Helmert09, p40. This impl takes O(N^2)"
  (iter (for c in conditions)
        (if (some #'variablep (cdr c))
            (collect c into lifted)
            (collect (normalize-fact-term c) into grounded))
        (finally
         (return
          (multiple-value-bind (decomposed temporary-rules)
              (join-ordering-aux lifted nil)
            (values (append grounded decomposed)
                    temporary-rules))))))

(defun join-ordering-aux (conditions acc)
  (if (<= (length conditions) 2)
      (values (mapcar #'normalize-fact-term conditions) acc)
      (multiple-value-bind (min-u min-c1 min-c2) (find-best-join conditions)
        (with-gensyms (tmp)
          (let ((new `(,tmp ,@min-u)))
            (join-ordering-aux
             (-<>> conditions
               (remove min-c1 arrow-macros:<> :test #'equal)
               (remove min-c2 arrow-macros:<> :test #'equal)
               (cons new))
             (list* `(:- ,new
                         ;; min-c1 has larger arity; put min-c2 first
                         ,(normalize-fact-term min-c2)
                         ,(normalize-fact-term min-c1))
                    acc)))))))

;;; relaxed-reachability

(defun tabled (rules)
  (ematch (first rules)
    ((or `(:- (,name) ,@_)
         `(,name))
     (error "Rule not normalized: zero-ary predicate ~a" name))
    ((or `(:- (,name ,@params) ,@_)
         `(,name ,@params))
     (let ((str (symbol-name name)))
       (assert (or (search str "-F" :start1 (- (length str) 2))
                   (search str "-O" :start1 (- (length str) 2))
                   (member name '(op fact))
                   (tmp-p `(,name ,@params)))
               nil
               "Rule not normalized: fact symbol not ending with -F: ~a" name)
       (cons `(:- (table (/ ,name ,(length params))))
             rules)))))

(defvar *reachable-facts*)
(defun register (rule)
  (let ((rule (normalize-fact-rule rule)))
    (ematch rule
      ((or `(:- (,name ,@_) ,@_)
           `(,name ,@_))
       (push rule (getf *reachable-facts* name))))))

(defvar *reachable-ops*)
(defun register-op (rule)
  (let ((rule (normalize-op-rule rule)))
    (ematch rule
      ((or `(:- (,name ,@_) ,@_)
           `(,name ,@_))
       (push rule (getf *reachable-ops* name))))))

(defun relaxed-reachability ()
  (append
   (iter (for (o . _) in *objects*)
         (collecting `(object ,o)))
   (let (*reachable-facts* *reachable-ops*)
     (register `(= ?o ?o))
     (mapcar #'register *init*)
     (iter (for a in *actions*)
           (ematch a
             ((plist :action name
                     :parameters params
                     :precondition `(and ,@precond)
                     :effect `(and ,@effects))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 precond)
                (mapcar #'register temporary-rules)
                (register-op
                 `(:- (,name ,@params)
                      ,@decomposed
                      ,@(iter (for p in params)
                              (collecting `(object ,p))))))
              (dolist (e effects)
                (match e
                  (`(forall ,vars (when (and ,@conditions) ,atom))
                    (when (positive atom)
                      (multiple-value-bind (decomposed temporary-rules)
                          (all-relaxed-reachable2 conditions)
                        (mapcar #'register temporary-rules)
                        (register
                         `(:- ,atom
                              ,(normalize-op-term `(,name ,@params))
                              ,@decomposed
                              ,@(iter (for p in vars)
                                      (collecting `(object ,p)))))))))))))
     (iter (for a in *axioms*)
           (ematch a
             ((list :derived predicate `(and ,@body))
              (multiple-value-bind (decomposed temporary-rules)
                  (all-relaxed-reachable2 body)
                (mapcar #'register temporary-rules)
                (register
                 `(:- ,predicate
                      ,@decomposed
                      ,@(iter (for p in (cdr predicate))
                              ;; parameters not referenced in the condition
                              (collecting `(object ,p)))))))))
     (iter (for p in *predicates*)
           (when (not (eq (car p) '=))
             (register `(:- ,p ! fail))))
     (append
      (mappend (lambda (ro) (tabled (nreverse (cdr ro)))) (plist-alist *reachable-ops*))
      (mappend (lambda (rf) (tabled (nreverse (cdr rf)))) (plist-alist *reachable-facts*))
      ;; output facts/ops
      `((:- relaxed-reachability
            (write ":facts\\n")
            (wrap
             (and ,@(iter (for p in *predicates*)
                          (when (added-p p)
                            (collecting
                             `(forall ,(normalize-fact-term p) (print-sexp ,p)))))))
            (write ":ops\\n")
            (wrap
             (and ,@(iter (for a in *actions*)
                          (ematch a
                            ((plist :action name
                                    :parameters params)
                             (collecting
                              `(forall ,(normalize-op-term `(,name ,@params))
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

