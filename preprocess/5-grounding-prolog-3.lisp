
#|

This is a rewrite of 5-grounding-prolog with minimally using the lifted predicates.

|#

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun ground (info &optional (package (find-package :pddl)))
  (with-parsed-information2 info
    (let ((result (%ground)))
      (let ((result2 (let ((*package* package))
                       (read-from-string result))))
        #+(or)
        (append (iter (for x in result2)
                      (collecting
                       (if (listp x)
                           (remove-duplicates x :test 'equal)
                           x)))
                ;; this remove-duplicates could be quite time consuming
                info)
        ;; result2 may contain duplicates, but we leave it at the moment
        ;; since we put them in an index and a trie
        (append result2 info)))))

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
  (assert (not (tmp-p term)))
  (ematch term
    ((list name)
     `(op ,name))
    ((list* name args)
     `(,(symbolicate name '-o) ,@args))))

(defun normalize-op-rule (rule)
  "Normalize the head of the op rule. Body should be normalized separately"
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-op-term head) ,@body))
    (_
     (normalize-op-term rule))))

(defun normalize-effect-term (term i)
  (assert (not (tmp-p term)))
  (ematch term
    ((list name)
     `(eff ,name ,i))
    ((list* name args)
     `(,(symbolicate name '-e) ,i ,@args))))

(defun normalize-effect-rule (rule i)
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-effect-term head i) ,@body))
    (_
     (normalize-effect-term rule i))))

(defun normalize-init-term (term)
  (assert (not (tmp-p term)))
  (ematch term
    ((list name)
     `(init ,name))
    ((list* name args)
     `(,(symbolicate name '-i) ,@args))))

(defun normalize-init-rule (rule)
  "Normalize the head of the fact rule. Body should be normalized separately"
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-init-term head) ,@body))
    (_
     (normalize-init-term rule))))

(defun normalize-del-term (term)
  (assert (not (tmp-p term)))
  (ematch term
    ((list name)
     `(del ,name))
    ((list* name args)
     `(,(symbolicate name '-d) ,@args))))

(defun normalize-del-rule (rule)
  "Normalize the head of the fact rule. Body should be normalized separately"
  (ematch rule
    (`(:- ,head ,@body)
      `(:- ,(normalize-del-term head) ,@body))
    (_
     (normalize-del-term rule))))

;; heads are normalzied in the last step

;;; join ordering

(defvar *inequality*)

(defun all-relaxed-reachable2 (conditions)
  (iter (for c in conditions)
        (ematch c
          (`(= ,@_)
            (collect c into equality))
          (`(not (= ,x ,y))
            (collect (cons x y) into inequality))
          (`(not ,_)
            ;; do nothing
            )
          (_
           (if (some #'variablep (cdr c))
               (collect c into lifted)
               (collect (normalize-fact-term c) into grounded))))
        (finally
         (let ((*inequality* inequality))
           (multiple-value-bind (decomposed temporary-rules) (join-ordering lifted)
             (return
               (values (append equality grounded decomposed)
                       temporary-rules)))))))

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

(defvar *instantiated*)

(defun join-ordering (conditions)
  "Helmert09, p40. This impl takes O(N^2)"
  (let ((*instantiated* nil))
    (join-ordering-aux conditions nil)))

(defun join-ordering-aux (conditions acc)
  (if (<= (length conditions) 2)
      ;; (values (mapcar #'normalize-fact-term conditions) acc)
      (values
       ;; no longer generate tmp predicates, for handling of inequality correctly
       (labels ((expand (condition)
                  (if (tmp-p condition)
                      (mappend #'expand (cddr (find condition acc :key 'second :test 'equal)))
                      (list* (normalize-fact-term condition)
                             (instantiated-inequality condition)))))
         (mappend #'expand conditions))
       nil)
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
                         ,min-c1
                         ,min-c2)
                    acc)))))))

(defun instantiated-inequality (condition)
  "Returns a set of inequality predicates (/= x y) that can be checked after the condition.
As a side effect, special variable *instantiated* is extended with newly instantiated variables,
and the consumed inequality conditions are removed from *inequality*."
  (let ((instantiated (union *instantiated* (remove-if-not #'variablep (cdr condition)))))
    (iter (for (x . y) in *inequality*)
          (if (and (or (not (variablep x)) (member x instantiated))
                   (or (not (variablep y)) (member y instantiated)))
              (collecting `(\\= ,x ,y) into result)
              (collecting (cons x y) into keep))
          (finally
           (setf *instantiated* instantiated
                 *inequality* keep)
           (return result)))))

;;; relaxed-reachability

(defun tabled (rules)
  (ematch (first rules)
    ((or `(:- (,name) ,@_)
         `(,name))
     (error "Rule not normalized: zero-ary predicate ~a" name))
    ((or `(:- (,name ,@params) ,@_)
         `(,name ,@params))
     (let ((str (symbol-name name)))
       (assert (or (ends-with-subseq "-F" str)
                   (ends-with-subseq "-E" str)
                   (ends-with-subseq "-I" str)
                   (ends-with-subseq "-D" str)
                   (ends-with-subseq "-O" str)
                   (member name '(op
                                  eff
                                  init
                                  del
                                  fact))
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

(defvar *reachable-effects*)
(defun register-effect (rule i)
  (let ((rule (normalize-effect-rule rule i)))
    (ematch rule
      ((or `(:- (,name ,@_) ,@_)
           `(,name ,@_))
       (push rule (getf *reachable-effects* name))))))

(defvar *initial-facts*)
(defun register-init (rule)
  (let ((rule (normalize-init-rule rule)))
    (ematch rule
      ((or `(:- (,name ,@_) ,@_)
           `(,name ,@_))
       (push rule (getf *initial-facts* name))))))

(defvar *deletable-facts*)
(defun register-deleted (rule)
  (let ((rule (normalize-del-rule rule)))
    (ematch rule
      ((or `(:- (,name ,@_) ,@_)
           `(,name ,@_))
       (push rule (getf *deletable-facts* name))))))

(defun relaxed-reachability ()
  "Returns a cl-prolog2 program that prints the reachable facts/ops"
  (append
   (iter (for (o . _) in *objects*)
         (collecting `(object ,o)))
   (let (*reachable-facts*
         *reachable-effects*
         *initial-facts*
         *deletable-facts*
         *reachable-ops*)
     (mapcar #'register *init*)
     (mapcar #'register-init *init*)
     (register-ops)
     (register-axioms)
     (iter (for p in *predicates*)
           (when (not (eq (car p) '=))
             (register-deleted `(:- ,p ! fail))
             (register-init `(:- ,p ! fail))
             (register `(:- ,p ! fail))))
     (append
      (mappend (lambda (rules) (tabled (nreverse (remove-duplicates (cdr rules) :test 'equal)))) (plist-alist *reachable-ops*))
      (mappend (lambda (rules) (tabled (nreverse (remove-duplicates (cdr rules) :test 'equal)))) (plist-alist *reachable-facts*))
      (mappend (lambda (rules) (tabled (nreverse (remove-duplicates (cdr rules) :test 'equal)))) (plist-alist *reachable-effects*))
      (mappend (lambda (rules) (tabled (nreverse (remove-duplicates (cdr rules) :test 'equal)))) (plist-alist *deletable-facts*))
      (mappend (lambda (rules) (nreverse (remove-duplicates (cdr rules) :test 'equal))) (plist-alist *initial-facts*))
      ;; output facts/ops
      `((:- relaxed-reachability
            (write ":facts\\n")
            (wrap
             (and ,@(iter (for p in *predicates*)
                          (when (added-p p)
                            (collecting
                                ;; this prints goal as (goal)
                                `(forall ,(normalize-fact-term p) (print-sexp ,(ensure-zeroary-to-list p))))))))
            ;; note: reachable atoms = (union added init) = (union generic monotonic+ init)
            (write ":ground-axioms\\n")
            (wrap
             (and ,@(iter (for a in *axioms*)
                          (ematch a
                            ((list :derived p _)
                             (collecting
                                 ;; this prints goal as (goal)
                                 `(forall ,(normalize-fact-term p) (print-sexp ,(ensure-zeroary-to-list p)))))))))
            (write ":ops\\n")
            (wrap
             (and ,@(iter (for a in *actions*)
                          (ematch a
                            ((plist :action name
                                    :parameters params)
                             (let ((p `(,name ,@params)))
                               (collecting
                                   `(forall ,(normalize-op-term p)
                                            (and (findall ?i ,(normalize-effect-term p '?i) ?list)
                                                 ;; this prints goal as (goal)
                                                 (print-sexp (list ,(ensure-zeroary-to-list p) ?list)))))))))))))))))

(defun no-op-constraints (effects)
  "adding the constraint which prunes a combination of parameters when
the effect lists are structurally equivalent"
  (unless *enable-no-op-pruning* (return-from no-op-constraints nil))
  (when (< (length effects) 2) (return-from no-op-constraints nil))

  (let (pos neg)
    (iter (for e in effects)
          (ematch e
            (`(forall ,_ (when ,_ (increase ,@_)))
              ;; ignore
              )
            (`(forall ,_ (when ,_ (not ,e)))
              (push e neg))
            (`(forall ,_ (when ,_ ,e))
              (push e pos))))
    (with-gensyms (?pos1 ?neg1 ?pos2 ?neg2)
      ;; ?pos, ?neg: unique set of add/delete effects,
      ;; including free universally-quantified variables
      `((remove-subsumed (list ,@pos) ,?pos1)
        (remove-subsumed (list ,@neg) ,?neg1)
        (predsort effect-order ,?pos1 ,?pos2)
        (predsort effect-order ,?neg1 ,?neg2)
        (\\=@= ,?pos2 ,?neg2)))))

(defun effect-order ()
  "ordering predicate for predsort which removes structurally equivalent terms as duplicates,
and also orders the terms by 'structure ordering' --- e.g.
 (X,X,Y) < (X,Y,Y) because they are reassigned to (0,0,1) < (0,1,1)
 (X,X,Y) < (Y,X,X) because they are reassigned to (0,0,1) < (0,1,1)
 (X,X,Y) < (A,B,C) because they are reassigned to (0,0,1) < (0,1,2)
"
  `((:- (effect-order = ?a ?b) (=@= ?a ?b))
    (:- (effect-order < ?ac ?bc)
        (copy_term ?ac ?a)
        (copy_term ?bc ?b)
        (numbervars ?a) (numbervars ?b)
        ;; (if (@< ?a ?b) (= ?r true) (= ?r false))
        ;; (write ---) (write_canonical (list ?a ?b ?r)) nl
        (@< ?a ?b)
        )
    (:- (effect-order > ?ac ?bc)
        (copy_term ?ac ?a)
        (copy_term ?bc ?b)
        (numbervars ?a) (numbervars ?b)
        ;; (if (@< ?a ?b) (= ?r true) (= ?r false))
        ;; (write ---) (write_canonical (list ?a ?b ?r)) nl
        (@> ?a ?b))))

(defun remove-subsumed ()
  "Removes the terms that are subsumed when variables as universally quantified, e.g. a(X,Y,Z) subsumes a(X,Y,Y)."
  `((:- (remove-subsumed (list) (list)) !)
    (:- (remove-subsumed (list ?e) (list ?e)) !)
    (:- (remove-subsumed (list* ?e ?rest) ?result)
        (remove-subsumed ?rest ?result2)
        (remove-subsumed-aux ?e ?result2 ?result2 (list) ?result))
    
    (remove-subsumed-aux ?e1 ? (list) ?acc (list* ?e1 ?acc))
    
    (:- (remove-subsumed-aux ?e1 ?orig (list* ?e2 ?) ? ?orig)
        (subsumes_term ?e2 ?e1))
    
    (:- (remove-subsumed-aux ?e1 ?orig (list* ?e2 ?rest) ?acc ?result)
        (subsumes_term ?e1 ?e2)
        (remove-subsumed-aux ?e1 ?orig ?rest ?acc ?result))
    
    (:- (remove-subsumed-aux ?e1 ?orig (list* ?e2 ?rest) ?acc ?result)
        (remove-subsumed-aux ?e1 ?orig ?rest (list* ?e2 ?acc) ?result))))

(defun register-ops ()
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
                   ;; ensure all parameters are grounded
                   ,@(iter (for p in params)
                           (collecting `(object ,p)))
                   ,@(no-op-constraints effects)
                   ,@(negative-conditions-satisfiable precond))))
           (iter (for e in effects) (for i from 0)
             (match e
               ;; TODO: (FORALL VARS ...) is not really necessary.
               ;; we can leave VARS as free variables.
               (`(forall ,_ (when (and ,@conditions) ,atom))
                 (when (positive atom)
                   (multiple-value-bind (decomposed temporary-rules)
                       (all-relaxed-reachable2 conditions)
                     (mapcar #'register temporary-rules)
                     (register
                      `(:- ,atom
                           ,(normalize-effect-term `(,name ,@params) i)))
                     (register-effect
                      `(:- (,name ,@params)
                           ,(normalize-op-term `(,name ,@params))
                           ,@decomposed
                           ;; ensure all parameters are grounded
                           ,@(iter (for p in (cdr atom))
                                   (collecting `(object ,p)))
                           ,@(negative-conditions-satisfiable conditions)) i)))
                 (when (negative atom)
                   (multiple-value-bind (decomposed temporary-rules)
                       (all-relaxed-reachable2 conditions)
                     (mapcar #'register temporary-rules)
                     (register-deleted
                      `(:- ,(second atom)
                           ,(normalize-effect-term `(,name ,@params) i)))
                     (register-effect
                      `(:- (,name ,@params)
                           ,(normalize-op-term `(,name ,@params))
                           ,@decomposed
                           ;; ensure all parameters are grounded
                           ,@(iter (for p in (cdr (second atom)))
                                   (collecting `(object ,p)))
                           ,@(negative-conditions-satisfiable conditions)) i))))))))))

;; note: negated axiom should not exist in the axiom body
;; See PDDL2.2 paper, p4, third item

(defun register-axioms ()
  (iter (for a in *axioms*)
        (ematch a
          ((list :derived `(,name ,@params) `(and ,@body))
           (if (and (null body) (null params))
               ;; when an axiom is a zero-ary axiom without body, register it as a fact
               (register `(,name ,@params))
               (multiple-value-bind (decomposed temporary-rules)
                   (all-relaxed-reachable2 body)
                 (mapcar #'register temporary-rules)
                 (register
                  `(:- (,name ,@params)
                       ,@decomposed
                       ;; ensure all parameters are grounded
                       ,@(iter (for p in params)
                               (collecting `(object ,p)))
                       ,@(negative-conditions-satisfiable body)))))

           ;; prove the negation of the body, (not (and body...)) = (or (not body)...)
           (when body
             (let ((neg-body (mapcar (compose #'to-nnf #'negate) body)))
               (register-deleted
                `(:- (,name ,@params)
                     (or ,@(let ((disjunctions
                                  (append (iter (for p in neg-body)
                                                ;; positive clauses (turned from negative) are handled here
                                                (when (positive p) (collecting (normalize-fact-term p))))
                                          ;; negative clauses (turned from positive) are handled here
                                          (negative-conditions-satisfiable neg-body))))
                             (iter (for d in disjunctions)
                                   (collecting
                                    `(and ,@(iter (for v in (free d))
                                                  (collecting `(object ,v)))
                                          ,d)))))
                     ;; ensure all parameters are grounded
                     ,@(iter (for p in params)
                             (collecting `(object ,p)))
                     ,@(negative-conditions-satisfiable body)))))))))

(defun negative-conditions-satisfiable (conditions)
  (when *enable-negative-precondition-pruning*
    (iter (for p in conditions)
          (when (negative p)
            (let ((atom (second p)))
              (cond
                ((axiom-p atom)
                 (when *enable-negative-precondition-pruning-for-axioms*
                   (collecting
                       (normalize-del-term atom))))
                ((eq (car atom) '=)
                 ;; skip
                 )
                (t
                 (when *enable-negative-precondition-pruning-for-fluents*
                   (collecting
                       `(or (not ,(normalize-init-term atom))
                            ,(normalize-del-term atom)))))))))))

(defun %ground ()
  (run-prolog
   (append (when (eq :swi *grounding-prolog*)
             `((:- (use_module (library tabling))) ; swi specific
               (:- (style_check (- singleton)))))
           ;; (effect-order)
           ;; (remove-subsumed)
           (print-sexp)
           `((:- (wrap ?goal)
                 (write "(")
                 (call ?goal)
                 (write ")"))
             (:- (if ?a ?b ?c)
                 (or (-> ?a ?b) ?c))
             (:- main
                 (wrap relaxed-reachability)
                 halt))
           (relaxed-reachability))
   *grounding-prolog* :args '("-g" "main") :input nil))

