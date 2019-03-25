;;; parser : implementation of PDDL normalization
;; Malte Helmert. Concise finite-domain representations for PDDL planning tasks, JAIR, 2009
;; See Section 4

(in-package :alien)

(defun find-domain (problem-path)
  (log:debug "finding the domain file...")
  (block nil
     (let ((dpath (make-pathname :defaults problem-path :name "domain")))
       (when (probe-file dpath) (log:debug "found! ~a" dpath) (return dpath)))
     (let ((dpath (make-pathname :defaults problem-path :name
                                 (format nil "~a-domain" (pathname-name problem-path)))))
       (when (probe-file dpath) (log:debug "found! ~a" dpath) (return dpath)))
     (error "~& Failed to infer the domain pathname from problem pathname!~%Problem: ~a~%Candidate: ~a~%Candidate: ~a"
            problem-path
            (make-pathname :defaults problem-path :name "domain")
            (make-pathname :defaults problem-path :name (format nil "~a-domain" (pathname-name problem-path))))))

(defun parse (problem &optional (domain (find-domain problem)))
  (parse0 domain problem))

;;; parse0

(defun parse0 (domain problem)
  (parse1
   (let ((*package* (find-package :pddl)))
     (with-open-file (s domain)
       (read s)))
   (let ((*package* (find-package :pddl)))
     (with-open-file (s problem)
       (read s)))))

;;; parse1

(named-readtables:in-readtable :fare-quasiquote)

(defun parse1 (domain problem)
  (ematch* (domain problem)
    ((`(define (domain ,domain) ,@domain-body)
       `(define (problem ,_)
          (:domain ,(eq domain))
          ,@problem-body))
     (parse2 domain-body problem-body))))

;;; parse2 --- typed lists are flattened

(defvar *types*)
(defvar *objects*)
(defvar *predicates*)
(defvar *predicate-types*)
(defvar *actions*)
(defvar *axioms*)
(defvar *init*)
(defvar *goal*)

(defun parse2 (domain problem)
  (let (*types*
        *objects*
        (*predicates* `((= ?o1 ?o2)))
        (*predicate-types* `((= object object)))
        *actions*
        *axioms*
        *init*
        *goal*)
    (grovel-types domain)
    (grovel-constants domain)
    (grovel-objects problem)
    (grovel-predicates domain)
    (grovel-init problem)
    (grovel-goal problem)
    (grovel-actions domain)
    (grovel-axioms domain)
    (parse3)))

(defun parse-typed-def (list)
  "Parse [objs* - type]* list. Does not handle the type inheritance.
 Returns an alist of (parameter . type).
 Untyped parameters are given the type OBJECT."
  (let (db
        buffer)
    (labels ((add (s type)
               (push (cons s type) db))
             (rec (list)
               (match list
                 ((list* '- (list* 'either _) _)
                  (error "EITHER type not currently supported"))
                 
                 ((list* '- type rest)
                  (setf buffer (nreverse buffer))
                  (dolist (s buffer)
                    (add s type))
                  (setf buffer nil)
                  (rec rest))
                 
                 (nil
                  (setf buffer (nreverse buffer))
                  (dolist (s buffer)
                    (add s 'object))
                  (nreverse db))
                 
                 ((list* now rest)
                  (push now buffer)
                  (rec rest)))))
      (rec list))))

(defun grovel-types (domain)
  (match domain
    ((assoc :types typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *types* parsed)
       (iter (for (type . supertype) in parsed)
             (when (not (or (eq supertype 'object)
                            (assoc supertype *types*)))
               (warn "connecting the orphan supertype ~a to object" supertype)
               (push (cons supertype 'object) *types*)))
       (appendf *predicates*
                (mapcar (lambda-ematch
                          ((cons type _) `(,type ?o)))
                        *types*))
       (appendf *predicate-types*
                (iter (for (current . parent) in *types*)
                      (collecting
                       `(,current ,parent))))))))

(defun flatten-type (type)
  "Returns the list of all parent types (including itself and OBJECT),
 handling the infinite loop.
Signals an error when the type is not connected to the root OBJECT type."
  (let (acc)
    (labels ((rec (type)
               (unless (member type acc)
                 (push type acc)
                 (iter (for (current . parent) in *types*)
                       (with parent-found = nil)
                       (when (eq current type)
                         (setf parent-found t)
                         (rec parent))
                       (finally
                        (when (not parent-found)
                          (when (not (eq 'object type))
                            (error "Type ~a is disconnected from the root OBJECT type!~%Inserting dependency to OBJECT" type)
                            (pushnew 'object acc))))))))
      (rec type)
      acc)))

(defun flatten-types/argument (arg type)
  "Returns a list of type predicates for each parent type of TYPE,
 with ARG as the argument."
  (iter (for parent in (flatten-type type))
        (unless (eq parent 'object)
          (collecting `(,parent ,arg)))))

(defun flatten-types/predicate (predicate &optional include-parent-types)
  "Look up the *predicate-types* and returns a list of type predicates and the
original predicate."
  (ematch predicate
    ((list* name args)
     (list* predicate
            (if include-parent-types
                (mappend #'flatten-types/argument
                         args
                         (cdr (or (assoc name *predicate-types*)
                                  (error "Predicate type for ~a is missing!" name))))
                (remove 'object
                        (mapcar #'list 
                                (cdr (or (assoc name *predicate-types*)
                                         (error "Predicate type for ~a is missing!" name)))
                                args)
                        :key #'first))))))

(defun flatten-typed-def (typed-def)
  "Takes a typed-def L and returns three values:
1. the untyped version of L
2. a list of literals converted from the types of the parameters, including the parent types
3. alist of (arg . type)

 Example: (?x - table) -> (?x), ((table ?x)), ((?x . table)) "
  (let* ((parsed (parse-typed-def typed-def))
         (w/o-type (mapcar #'car parsed))
         (type-conditions
          (iter (for (arg . type) in parsed)
                (unless (eq type 'object)
                  (collecting `(,type ,arg))))))
    (values w/o-type type-conditions parsed)))

(defun flatten-types/condition (condition)
  (ematch condition
    (nil
     `(and))
    ((list 'exists params condition)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(exists ,w/o-type
               (and ,@type-conditions
                    ,(flatten-types/condition condition)))))
    ((list 'forall params condition)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(forall ,w/o-type
                (imply (and ,@type-conditions)
                       ,(flatten-types/condition condition)))))
    ((list* (and kind (or 'and 'or 'not 'imply))
            conditions)
     `(,kind ,@(mapcar #'flatten-types/condition conditions)))
    (_
     `(and ,@(flatten-types/predicate condition)))))

(defun flatten-types/effect (effect)
  (ematch effect
    (nil
     `(and))
    ((list* (and kind (or 'or 'exists)) _)
     (error "~a should not appear in the effects: ~a" kind effect))
    (`(forall ,params ,effect)
     (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
       `(forall ,w/o-type
                (when (and ,@type-conditions)
                  ,(flatten-types/effect effect)))))
    (`(when ,condition ,body)
      `(when ,(flatten-types/condition condition)
         ,(flatten-types/effect body)))
    (`(and ,@conditions)
      `(and ,@(mapcar #'flatten-types/effect conditions)))
    (`(increase ,@_)
      (log:trace "skipping ~a" effect)
      `(and))
    (_ effect)))

(defun grovel-constants (domain)
  (match domain
    ((assoc :constants typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *objects* parsed)
       (dolist (pair parsed)
         (match pair
           ((cons o type)
            (appendf *init* (flatten-types/argument o type)))))))))

(defun grovel-objects (problem)
  ;; almost the same as grovel-constants
  (match problem
    ((assoc :objects typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *objects* parsed)
       (dolist (pair parsed)
         (match pair
           ((cons o type)
            (appendf *init* (flatten-types/argument o type)))))))))

(defun grovel-predicates (domain)
  (match domain
    ((assoc :predicates predicates)
     (dolist (predicate predicates)
       (match predicate
         (`(,name ,@typed-def)
           (multiple-value-bind (w/o-type type-conditions parsed) (flatten-typed-def typed-def)
             (declare (ignore type-conditions))
             (push `(,name ,@w/o-type) *predicates*)
             (push `(,name ,@(mapcar #'cdr parsed)) *predicate-types*))))))))

(defun grovel-init (problem)
  (ematch problem
    ((assoc :init predicates)
     (appendf *init*
              (iter (for condition in predicates)
                    (ematch condition
                      ((list* '= _)
                       (log:trace "skipping ~a" condition))
                      ((list* 'not _)
                       (log:trace "skipping ~a -- closed world assumption" condition))
                      (_
                       (appending (flatten-types/predicate condition t))))))
     (setf *init* (remove-duplicates *init* :test 'equal)))))

(defun grovel-goal (problem)
  (ematch problem
    ((assoc :goal (list condition))
     ;; goal may contain forall etc. so it needs flattening
     (setf *goal* (flatten-types/condition condition)))))

(defun grovel-actions (domain)
  (dolist (it domain)
    (match it
      ((list* :action name
              (plist :parameters params
                     :precondition pre :effect eff))
       (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
         (push (unify-duplicates
                w/o-type
                (lambda (newparams unifiers)
                  `(:action ,name
                            :parameters ,newparams
                            :precondition
                            (and ,@type-conditions
                                 ,@unifiers
                                 ,(flatten-types/condition pre))
                            :effect ,(flatten-types/effect eff))))
               *actions*)))
       ((list* something _)
        ;; (log:info "ignoring (~s ...)" something)
        ))))

(defun grovel-axioms (domain)
  (dolist (it domain)
    (match it
       ((list (or :derived :axiom) (list* predicate params) condition)
        (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
          (push (unify-duplicates
                 w/o-type
                 (lambda (newparams unifiers)
                   `(:derived (,predicate ,@newparams)
                              (and ,@type-conditions
                                   ,@unifiers
                                   ,(flatten-types/condition condition)))))
                *axioms*)))
       ((list* something _)
        ;; (log:info "ignoring (~s ...)" something)
        ))))

(defun new-variable ()
  "Interns a new symbol in ARRIVAL.PDDL"
  ;; The use of gentemp instead of gensym is intentional; Since the file was already read,
  ;; it is safe to use gentemp
  (gentemp "?" :arrival.pddl))

(defun unify-duplicates (variables fn)
  "If there are any duplicates in VARIABLES,
the second or later appearances are replaced with a new symbol.
FN is called with two arguments; The new, unique variable list
and a list of equality constraints.
"
  (match variables
    (nil
     (funcall fn nil nil))
    
    ((list* v rest)
     (if (find v rest)
         (let ((new (new-variable)))
           (warn "Found a duplicated parameter ~a, translated as equality constraints" v)
           (unify-duplicates
            (substitute new v rest)
            (lambda (newparams unifiers)
              (funcall fn
                       (cons v newparams)
                       (cons `(= ,v ,new) unifiers)))))
         (unify-duplicates
          rest
          (lambda (newparams unifiers)
            (funcall fn
                     (cons v newparams)
                     unifiers)))))))

;;; parse3 --- convert conditions to NNF (i.e. NOT appears on leafs only), compiling IMPLY away

(defvar *actions3*)
(defvar *axioms3*)
(defvar *init3*)
(defvar *goal3*)

(defun parse3 ()
  (let (*actions3*
        *axioms3*
        *init3*
        *goal3*)
    (nnf-actions)
    (nnf-axioms)
    (nnf-init)
    (nnf-goal)
    (parse4)))

(defun negate (condition)
  (ematch condition
    ((list 'not c)
     c)
    ((list* 'and rest)
     `(or ,@(mapcar #'negate rest)))
    ((list* 'or rest)
     `(and ,@(mapcar #'negate rest)))
    ((list 'forall args body)
     `(exists ,args ,(negate body)))
    ((list 'exists args body)
     `(forall ,args ,(negate body)))
    ((list* (or 'when 'increase) _)
     (error "negating conditional effects / fluents not allowed"))
    (_
     `(not ,condition))))

(defun to-nnf (condition)
  (simplify-nnf (%to-nnf condition)))

(defun %to-nnf (condition)
  (ematch condition
    (`(imply ,a ,b)
      `(or ,(%to-nnf `(not ,a))
           ,(%to-nnf b)))
    (`(not (imply ,a ,b))
      `(and ,(%to-nnf a)
            ,(%to-nnf `(not ,b))))
    
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'%to-nnf rest)))
    ((list (and kind (or 'forall 'exists)) args body)
     `(,kind ,args ,(%to-nnf body)))
    ((list 'when condition body)
     `(when ,(%to-nnf condition)
        ,(%to-nnf body)))
    ((list 'not (and a (list* (or 'and 'or 'forall 'exists 'not) _)))
     (%to-nnf (negate a)))
    (`(not (when ,_ ,_))
      (error "(not (when ...)) should not happen! ~%~A" condition))
    ((list 'not _)
     condition)
    (_
     condition)))

(defun %merge-same-clauses (type forms)
  (iter (for elem in forms)
        (match elem
          ((list* (eq type) subrest)
           (appending subrest))
          (_
           (collecting elem)))))

(defun simplify-nnf (form)
  "Remove some obvious constants / conflicts. The result does not contain:
Single compound forms:
 (and X), (or X)
Compound forms that contains true/false consants:
 (and ... (or) ... ) -> (or)
 (or ... (and) ... ) -> (and)
 (or ... X ... (not X) ... ) -> (and)
 (and ... X ... (not X) ... ) -> (or)
Duplicated forms:
 (and ... X ... X ... ) -> (and ... X ... ...)
 (or  ... X ... X ... ) -> (or  ... X ... ...)
"
  (ematch form
    ((list 'and) form)
    ((list 'or)  form)
    ((list 'and x) (simplify-nnf x))
    ((list 'or  x) (simplify-nnf x))
    ((list* 'and rest)
     (let* ((rest (mapcar #'simplify-nnf rest))
            (rest (%merge-same-clauses 'and rest)) ; (and) is eliminated here
            )
       (cond
         ((member '(or) rest :test 'equal)
          '(or))
         ((iter outer
                (for (c1 . rest2) on rest)
                (iter (for c2 in rest2)
                      (in outer
                          (thereis
                           (match c2
                             ((list 'not (equal c1)) t))))))
          '(or))
         (t
          (match (remove-duplicates rest :test 'equal)
            ((list x) x)
            (nil
             ;; happens when rest = '((and) (and)) is reduced by %merge-same-clauses
             `(and))
            (result
             (list* 'and result)))))))
    ((list* 'or rest)
     (let* ((rest (mapcar #'simplify-nnf rest))
            (rest (%merge-same-clauses 'or rest)) ;(or) is eliminated here
            )
       (cond
         ((member '(and) rest :test 'equal)
          '(and))
         ((iter outer
                (for (c1 . rest2) on rest)
                (iter (for c2 in rest2)
                      (in outer
                          (thereis
                           (match c2
                             ((list 'not (equal c1)) t))))))
          ;; (or ... A ... (not A) ...)
          '(and))
         (t
          (match (remove-duplicates rest :test 'equal)
            ((list x) x)
            (nil
             ;; happens when rest = '((or) (or)) is reduced by %merge-same-clauses
             `(or))
            (result
             (list* 'or result)))))))
    
    ((list (and kind (or 'forall 'exists)) args body)
     `(,kind ,args ,(simplify-nnf body)))
    ((list 'when condition body)
     `(when ,(simplify-nnf condition)
        ,(simplify-nnf body)))
    (_
     form)))

(defun nnf-actions ()
  (dolist (it *actions*)
    (push 
     (ematch it
       ((plist :action name :parameters params :precondition pre :effect eff)
        (list :action name :parameters params
              :precondition (to-nnf pre)
              :effect (to-nnf eff))))
     *actions3*)))

(defun nnf-axioms ()
  (dolist (it *axioms*)
    (push 
     (ematch it
       ((list :derived derived condition)
        (list :derived derived (to-nnf condition))))
     *axioms3*)))

(defun nnf-init ()
  (dolist (it *init*)
    (push
     (to-nnf it)
     *init3*)))

(defun nnf-goal ()
  (setf
   *goal3*
   (to-nnf *goal*)))

;;; parse4 --- (forall x y) -> (not (exists x (not y))) -> (not axiom), where axiom = (exists x (not y))

(defvar *actions4*)
(defvar *axioms4*)
(defvar *init4*)
(defvar *goal4*)

(defun parse4 ()
  (let (*actions4*
        *axioms4*
        *init4*
        *goal4*)
    (remove-universal-actions)
    (remove-universal-axioms)
    (remove-universal-init)
    (remove-universal-goal)
    (parse5)))

(defun variablep (variable)
  (ematch variable
    ((symbol :name (string* #\? _))
     t)
    ((symbol)
     nil)))

(defun free (formula)
  "Returns a list of free variables in FORMULA"
  (ematch formula
    ((list 'not pred)
     (free pred))
    ((list* (or 'and 'or) rest)
     (reduce #'union rest :key #'free :initial-value nil))
    ((list (or 'forall 'exists) args body)
     (set-difference (free body) args))
    ((list 'when condition body)
     (union (free condition)
            (free body)))
    ((list* _ args)
     (remove-if-not #'variablep args))))

(defun remove-forall/condition (condition)
  ;; assumption: inputs are already NNF, so no need to call TO-NNF
  (ematch condition
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'remove-forall/condition rest)))
    ((list 'forall args body)
     ;; BODY is an NNF 
     (with-gensyms (forall-axiom)
       (import forall-axiom :pddl)
       (let* ((e `(exists ,args ,(remove-forall/condition
                                  (to-nnf
                                   ;; negated body is not an NNF
                                   (negate body)))))
              (p (free e))
              (a `(,forall-axiom ,@p)))
         (push a *predicates*)
         (push `(:derived ,a ,e) *axioms4*)
         `(not ,a))))
    
    ((list 'exists args body)
     `(exists ,args ,(remove-forall/condition body)))
    
    ((list* (or 'when 'increase) _)
     (error "removing forall from conditional effects / fluents not allowed here"))
    (_ condition)))

(defun remove-forall/effect (condition)
  ;; assumption: inputs are already NNF, so no need to call TO-NNF
  (ematch condition
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'remove-forall/effect rest)))
    
    ((list (and kind (or 'forall 'exists)) args body)
     `(,kind ,args ,(remove-forall/effect body)))
    
    ((list 'when condition body)
     `(when ,(remove-forall/condition condition)
        ,(remove-forall/effect body)))
    (_ condition)))

(defun remove-universal-actions ()
  (dolist (it *actions3*)
    (push 
     (ematch it
       ((plist :action name :parameters params :precondition pre :effect eff)
        (list :action name :parameters params
              :precondition (remove-forall/condition pre)
              :effect (remove-forall/effect eff))))
     *actions4*)))

(defun remove-universal-axioms ()
  (dolist (it *axioms3*)
    (push 
     (ematch it
       ((list :derived derived condition)
        (list :derived derived (remove-forall/condition condition))))
     *axioms4*)))

(defun remove-universal-init ()
  (dolist (it *init3*)
    (push
     (remove-forall/condition it)
     *init4*)))

(defun remove-universal-goal ()
  (setf
   *goal4*
   ;; note: original FastDownward parser checks if this contains disjunctions/existential quantifier.
   ;; I neglect doing this.
   (with-gensyms (goal-axiom)
     (import goal-axiom :pddl)
     (push `(,goal-axiom) *predicates*)
     (push `(:derived (,goal-axiom) ,(remove-forall/condition *goal3*)) *axioms4*)
     `(,goal-axiom))))

;;; parse5 --- eliminate disjunctions

(defvar *actions5*)
(defvar *axioms5*)

(defun parse5 ()
  (let (*actions5*
        *axioms5*)
    (remove-disjunction-actions)
    (remove-disjunction-axioms)
    (parse6)))

(defun &nnf-dnf (condition)
  ;; now we have only and, or, exists, not, predicates.
  ;; OR clause is converted into an iterator.
  (ematch condition
    (`(or ,@conditions)
      (let ((&conditions (mapcar #'&nnf-dnf conditions)))
        (lambda (k)
          ;; calls k for each element
          (dolist (&now &conditions)
            (funcall &now k)))))

    (`(and ,@conditions)
      (let ((&conditions (mapcar #'&nnf-dnf conditions)))
        (lambda (k)
          (labels ((rec (&conditions stack)
                     (ematch &conditions
                       ((list* &first &more)
                        (funcall &first
                                 (lambda (result)
                                   (rec &more (cons result stack)))))
                       (nil
                        (funcall k `(and ,@stack))))))
            (rec &conditions nil)))))
    
    (`(exists ,args ,body)
      (let ((&body (&nnf-dnf body)))
        (lambda (k)
          (funcall &body
                   (lambda (result)
                     (funcall k `(exists ,args ,result)))))))
    
    (_
     (lambda (k) (funcall k condition)))))

(defun &nnf-dnf/effect (condition)
  ;; now we have only and, or, exists, not, predicates.
  ;; OR clause is converted into an iterator.
  (ematch condition
    (`(or ,@conditions)
      (let ((&conditions (mapcar #'&nnf-dnf/effect conditions)))
        (lambda (k)
          ;; calls k for each element
          (dolist (&now &conditions)
            (funcall &now k)))))

    (`(and ,@conditions)
      (let ((&conditions (mapcar #'&nnf-dnf/effect conditions)))
        (lambda (k)
          (labels ((rec (&conditions stack)
                     (ematch &conditions
                       ((list* &first &more)
                        (funcall &first
                                 (lambda (result)
                                   (rec &more (cons result stack)))))
                       (nil
                        (funcall k `(and ,@stack))))))
            (rec &conditions nil)))))
    
    (`(exists ,args ,body)
      (let ((&body (&nnf-dnf/effect body)))
        (lambda (k)
          (funcall &body
                   (lambda (result)
                     (funcall k `(exists ,args ,result)))))))

    (`(when ,condition ,effect)
      (let ((&condition (&nnf-dnf condition))
            (&effect (&nnf-dnf effect)))
        (lambda (k)
          (funcall &condition
                   (lambda (result1)
                     (funcall &effect
                              (lambda (result2)
                                (funcall k `(when ,result1 ,result2)))))))))
    
    (_
     (lambda (k) (funcall k condition)))))

(defun remove-disjunction-actions ()
  (dolist (it *actions4*)
    (ematch it
      ((plist :action name :parameters params :precondition pre :effect eff)
       (let ((&pre (&nnf-dnf pre))
             (&eff (&nnf-dnf/effect eff))
             (i -1))
         (funcall &pre
                  (lambda (pre)
                    (funcall &eff
                             (lambda (eff)
                               (incf i)
                               (when (member 'or (flatten eff))
                                 (break+ (list :action (let ((*package* (symbol-package name)))
                                                         ;; not specifying PDDL for testing convenience
                                                         (symbolicate name '@ (princ-to-string i)))
                                               :original-action name
                                               :parameters params
                                               :precondition pre
                                               :effect eff)
                                         it))
                               (push (list :action (let ((*package* (symbol-package name)))
                                                     ;; not specifying PDDL for testing convenience
                                                     (symbolicate name '@ (princ-to-string i)))
                                           :original-action name
                                           :parameters params
                                           :precondition pre
                                           :effect eff)
                                     *actions5*))))))))))

(defun remove-disjunction-axioms ()
  (dolist (it *axioms4*)
    (ematch it
       ((list :derived derived condition)
        (let ((&condition (&nnf-dnf condition)))
          (funcall &condition
                   (lambda (condition)
                     (push (list :derived derived condition)
                           *axioms5*))))))))

;;; parse6 --- move existential quantifier

(defvar *actions6*)
(defvar *axioms6*)

(defun parse6 ()
  (let (*actions6*
        *axioms6*)
    (move-exists-actions)
    (move-exists-axioms)
    (parse7)))

(defun move-exists/condition (condition)
  "move existential quantifier as well as flattening an AND tree"
  (let (args-acc body-acc)
    (labels ((rec (condition)
               (ematch condition
                 (`(and ,@conditions)
                   (map nil #'rec conditions))
                 (`(exists ,args ,body)
                   (let ((aliases (mapcar (lambda (a) (cons a (gensym (symbol-name a)))) args)))
                     ;; rewrite clauses
                     (labels ((rewrite (c)
                                (ematch c
                                  (`(and ,@rest)
                                    `(and ,@(mapcar #'rewrite rest)))
                                  (`(exists ,more-args ,body)
                                    `(exists ,more-args ,(rewrite body)))
                                  (`(not ,l)
                                    `(not ,(rewrite l)))
                                  (`(,predicate ,@args)
                                    `(,predicate ,@(mapcar (lambda (arg)
                                                             (or (cdr (assoc arg aliases))
                                                                 ;; when not found
                                                                 arg))
                                                           args))))))
                       (appendf args-acc (mapcar #'cdr aliases))
                       (rec (rewrite body)))))
                 
                 ((list* (or 'when 'increase) _)
                  (error "removing exists from conditional effects / fluents not allowed here"))

                 (_
                  (push condition body-acc)))))
      (rec condition)
      `(exists ,args-acc (and ,@body-acc)))))

(defun move-exists/effect (condition)
  "move existential quantifier as well as flattening an AND tree"
  (let (conjunction)
    (labels ((rec (condition)
               (ematch condition
                 (`(and ,@conditions)
                   (map nil #'rec conditions))
                 (`(when ,condition ,body)
                   ;; rewrite according to:
                   ;; (when (exists (?x) condition) effect) == (forall (?x) (when condition effect))
                   (let ((condition (move-exists/condition condition)))
                     (push
                      (match condition
                        (`(exists ,args ,condition)
                          `(forall ,args
                                   (when ,condition
                                     ,(move-exists/effect body))))
                        (_
                         `(when ,condition
                            ,(move-exists/effect body))))
                      conjunction)))
                 (`(forall ,args ,body)
                   (push
                    `(forall ,args ,(move-exists/effect body))
                    conjunction))
                 (`(exists ,@_)
                   (error "exists should not appear in the effects"))
                 (_
                  ;; not and predicates
                  (push condition conjunction)))))
      (rec condition)
      `(and ,@conjunction))))

(defun move-exists-actions ()
  (dolist (it *actions5*)
    (ematch it
      ((plist :action name :original-action name2
              :parameters params :precondition pre :effect eff)
       (push 
        (match (move-exists/condition pre)
          ;; remove exists
          (`(exists ,args ,condition)
            (list :action name
                  :original-action name2
                  :parameters (append params args)
                  :original-parameters params
                  :precondition condition
                  :effect (move-exists/effect eff)))
          (condition
           (list :action name
                 :original-action name2
                 :parameters params
                 :original-parameters params
                 :precondition condition
                 :effect (move-exists/effect eff))))
        *actions6*)))))

(defun move-exists-axioms ()
  (dolist (it *axioms5*)
    (ematch it
      ((list :derived result condition)
       (push (list :derived result
                   (match (move-exists/condition condition)
                     ;; remove exists
                     (`(exists ,_ ,condition) condition)
                     (condition condition)))
             *axioms6*)))))

;;; parse7 --- simplify effects

(defvar *actions7*)

(defun parse7 ()
  (let (*actions7*)
    (simplify-effects-actions)
    (parse8)))

(defun simplify-effects-actions ()
  (dolist (it *actions6*)
    (ematch it
      ((plist :action name :original-action name2
              :parameters params :original-parameters oparams
              :precondition pre :effect eff)
       (push
        (list :action name :original-action name2
              :parameters params :original-parameters oparams
              :precondition pre :effect (simplify-effect eff))
        *actions7*)))))

(defmacro foreach (list &body body)
  `(mapcar (lambda (_) ,@body) ,list))

(defun simplify-effect (effect)
  (let (acc)
    (labels ((rec (effect)
               ;; and -> forall -> when -> simple
               (ematch effect
                 ;; simplify AND
                 (`(and ,@rest)
                   (dolist (r rest)
                     (rec r)))
                 
                 ;; demote FORALL
                 (`(forall ,args (and ,@body))
                   (rec
                    `(and ,@(foreach body `(forall ,args ,_)))))
                 ;; combine FORALL
                 (`(forall ,args (forall ,args2 ,body))
                   (rec
                    `(forall (,@args ,@args2) ,body)))
                 (`(forall ,args ,body)
                   (multiple-value-bind (result expanded) (rec2 body)
                     (if expanded
                         (rec
                          `(forall ,args ,result))
                         (push effect acc))))))
             
             (rec2 (effect)
               (ematch effect
                 ;; demote WHEN
                 (`(when ,condition (and ,@body))
                   (values `(and ,@(foreach body `(forall () (when ,condition ,_))))
                           t))
                 ;; demote WHEN
                 (`(when ,condition (forall ,args ,body))
                   (values `(forall ,args (when ,condition ,body))
                           t))
                 ;; combine WHEN
                 (`(when ,condition (when ,condition2 ,body))
                   (values `(forall () (when ,(rec3 `(and ,condition ,condition2)) ,body))
                           t))
                 (`(when ,_ ,_)
                   effect)))
             
             (rec3 (condition)
               ;; simplify AND in the condition of WHEN
               (let (acc)
                 (labels ((rec4 (condition)
                            (match condition
                              (`(and ,@rest)
                                (map nil #'rec4 rest))
                              (_
                               (push condition acc)))))
                   (rec4 condition))
                 `(and ,@acc))))
      (rec `(forall () (when (and) ,effect)))
      `(and ,@acc))))

;;; parse8 --- remove-duplicates and output

(defun parse8 ()
  (list :type *types*
        :objects *objects*
        :predicates *predicates*
        :predicate-types *predicate-types*
        :init (remove-duplicates *init4* :test 'equal)
        :goal *goal4*
        :axioms
        (mapcar (lambda-ematch
                  (`(:derived ,predicate (and ,@body))
                    `(:derived ,predicate (and ,@(remove-duplicates body :test 'equal)))))
                *axioms6*)
        :actions
        (mapcar (lambda-ematch
                  ((plist :action name :original-action name2
                          :parameters params :original-parameters orig
                          :precondition `(and ,@precond)
                          :effect `(and ,@effects))
                   (list :action name
                         :original-action name2
                         :parameters params
                         :original-parameters orig
                         :precondition
                         `(and ,@(remove-duplicates precond :test 'equal))
                         :effect
                         `(and ,@(mapcar
                                  (lambda-ematch
                                    (`(forall ,vars (when (and ,@conditions) ,atom))
                                      `(forall ,vars (when (and ,@(remove-duplicates conditions :test 'equal))
                                                       ,atom))))
                                  effects)))))
                *actions7*)))

