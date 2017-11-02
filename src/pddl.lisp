;;; parser

(in-package :strips)

(defun parse (problem &optional (domain (find-domain problem)))
  (parse0 domain problem))


;;; parse0

(defun parse0 (domain problem)
  (parse1
   (with-open-file (s domain)
     (read s))
   (with-open-file (s problem)
     (read s))))

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
(defvar *actions*)
(defvar *axioms*)
(defvar *init*)
(defvar *goal*)

(defun parse2 (domain problem)
  (let (*types*
        *objects*
        *predicates*
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

(defun grovel-types (domain)
  (ematch domain
    ((assoc :types typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *types* parsed)
       (appendf *predicates*
                (mapcar (lambda-ematch
                          ((cons type _) `(,type ?o)))
                        parsed))
       (dolist (p parsed)
         (match p
           ((cons self (and parent (not 'object)))
            (push `(:derived (,parent ?o) (,self ?o)) *axioms*))))))))

(defun grovel-constants (domain)
  (match domain
    ((assoc :constants typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *objects* parsed)
       (dolist (pair parsed)
         (match pair
           ((cons o (and type (not 'object)))
            (push `(,type ,o) *init*))))))))

(defun grovel-objects (problem)
  (ematch problem
    ((assoc :objects typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *objects* parsed)
       (dolist (pair parsed)
         (match pair
           ((cons o (and type (not 'object)))
            (push `(,type ,o) *init*))))))))

(defun grovel-predicates (domain)
  (ematch domain
    ((assoc :predicates predicates)
     (dolist (predicate predicates)
       (match predicate
         (`(,name ,typed-def)
           (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def typed-def)
             (push `(,name ,@w/o-type) *predicates*)
             (dolist (condition type-conditions)
               (push `(:derived ,condition (,name ,@w/o-type)) *axioms*)))))))))

(defun grovel-init (problem)
  (ematch problem
    ((assoc :init predicates)
     (dolist (condition predicates)
       (ematch condition
         ((list* '= _)
          (format t "~&; skipping ~a" condition))
         (_
          ;; init is type-less from the beginning
          (push condition *init*)))))))

(defun grovel-goal (problem)
  (ematch problem
    ((assoc :goal condition)
     ;; goal may contain forall etc. so it needs flattening
     (setf *goal* (flatten-types condition)))))

(defun grovel-actions (domain)
  (dolist (it (remove-if-not (lambda-match ((list* :action _) t)) domain))
    (ematch it
      ((list :action name :parameters params
             :precondition pre :effect eff)
       (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
         (push `(:action ,name
                         :parameters ,w/o-type
                         :precondition
                         (and ,@type-conditions
                              ,(flatten-types pre))
                         :effect ,eff)
               *actions*))))))

(defun grovel-axioms (domain)
  (dolist (it (remove-if-not (lambda-match ((list* (or :derived :axiom) _) t)) domain))
    (push 
     (ematch it
       ((list :derived (list* predicate params) condition)
        (multiple-value-bind (w/o-type type-conditions) (flatten-typed-def params)
          (list :derived
                `(,predicate ,@w/o-type)
                `(and ,@type-conditions
                      ,(flatten-types condition))))))
     *axioms*)))

;;; parse3 --- convert conditions to NNF, compiling IMPLY away

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
    ((list 'exist args body)
     `(forall ,args ,(negate body)))
    ((list* (or 'when 'increase) _)
     (error "negating conditional effects / fluents not allowed"))
    (_
     `(not ,condition))))

(defun to-nnf (condition)
  (ematch condition
    (`(imply ,a ,b)
      `(or ,(to-nnf `(not ,a))
           ,(to-nnf b)))
    (`(not (imply ,a ,b))
      `(and ,(to-nnf a)
            ,(to-nnf `(not ,b))))
    
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'to-nnf rest)))
    ((list (and kind (or 'forall 'exist)) args body)
     `(,kind ,args ,(to-nnf body)))
    ((list 'when condition body)
     `(when ,(to-nnf condition)
        ,(to-nnf body)))
    ((list 'not (and a (list* (or 'and 'or 'forall 'exists 'not) _)))
     (to-nnf (negate a)))
    (`(not (when ,_ ,_))
      (error "(not (when ...)) should not happen! ~%~A" condition))
    ((list 'not _)
     condition)
    (_
     condition)))

(defun nnf-actions ()
  (dolist (it *actions*)
    (push 
     (ematch it
       ((list :action name :parameters params :precondition pre :effect eff)
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


;;; parse4 --- forall x y -> ( not exist not axiom, where y -> axiom )

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
  (ematch formula
    ((list 'not pred)
     (free pred))
    ((list* (or 'and 'or) rest)
     (reduce #'union rest :key #'free))
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
       (let* ((e `(exists ,args ,(remove-forall/condition
                                  (to-nnf
                                   ;; negated body is not an NNF
                                   (negate body)))))
              (p (free e))
              (a `(,forall-axiom ,@p)))
         (push `(:derived ,a ,e) *axioms4*)
         `(not ,a))))
    
    ((list 'exist args body)
     `(exist ,args ,(remove-forall/condition body)))
    
    ((list* (or 'when 'increase) _)
     (error "removing forall from conditional effects / fluents not allowed here"))
    (_ condition)))

(defun remove-forall/effect (condition)
  ;; assumption: inputs are already NNF, so no need to call TO-NNF
  (ematch condition
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'remove-forall/effect rest)))
    
    ((list (and kind (or 'forall 'exist)) args body)
     `(,kind ,args ,(remove-forall/effect body)))
    
    ((list 'when condition body)
     `(when ,(remove-forall/condition condition)
        ,(remove-forall/effect body)))
    (_ condition)))

(defun remove-universal-actions ()
  (dolist (it *actions3*)
    (push 
     (ematch it
       ((list :action name :parameters params :precondition pre :effect eff)
        (list :action name :parameters params
              :precondition (remove-forall/condition pre)
              :effect (remove-forall/effect eff))))
     *actions4*)))

(defun remove-universal-axioms ()
  (dolist (it *axioms*)
    (push 
     (ematch it
       ((list :derived derived condition)
        (list :derived derived (remove-forall/condition condition))))
     *axioms4*)))

(defun remove-universal-init ()
  (dolist (it *init*)
    (push
     (remove-forall/condition it)
     *init4*)))

(defun remove-universal-goal ()
  (setf
   *goal4*
   (with-gensyms (goal-axiom)
     (push `(:derived (,goal-axiom) ,(remove-forall/condition *goal*)) *axioms4*)
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
  ;; now we have only and, or, exist, not, predicates.
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

(defun collect-results (cps)
  (let (acc)
    (funcall cps
             (lambda (result)
               (push result acc)))
    (print acc)))

(defun nnf-dnf (condition)
  (collect-results (&nnf-dnf condition)))

(progn
  (nnf-dnf 'a)
  (nnf-dnf '(or a b))
  (nnf-dnf '(or a b c (or d e)))
  (nnf-dnf '(and x (or a b)))
  (nnf-dnf '(and (or x y) (or a b)))
  (nnf-dnf '(and (or x y) c (or a b)))
  (nnf-dnf '(and (or (and x z) y) c (or a b)))
  (nnf-dnf '(or (and x (or w z)) y))

  (nnf-dnf '(and (clear ?x)
             (or (clear ?w)
              (not (clear ?z)))))

  (nnf-dnf '(exists (?x ?y)
             (or (clear ?x)
              (not (clear ?y))))))

(defun &nnf-dnf/effect (condition)
  ;; now we have only and, or, exist, not, predicates.
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


(defun nnf-dnf/effect (condition)
  (collect-results (&nnf-dnf/effect condition)))

(progn
  (nnf-dnf/effect 'a)
  (nnf-dnf/effect '(or a b))
  (nnf-dnf/effect '(or a b c (or d e)))
  (nnf-dnf/effect '(and x (or a b)))
  (nnf-dnf/effect '(and (or x y) (or a b)))
  (nnf-dnf/effect '(and (or x y) c (or a b)))
  (nnf-dnf/effect '(and (or (and x z) y) c (or a b)))
  (nnf-dnf/effect '(or (and x (or w z)) y))

  (nnf-dnf/effect '(and (clear ?x)
                    (or (clear ?w)
                     (not (clear ?z)))))

  (nnf-dnf/effect '(exists (?x ?y)
                    (or (clear ?x)
                     (not (clear ?y)))))

  (nnf-dnf/effect `(when (or a b)
                     (and c d (or e f)))))

(defun remove-disjunction-actions ()
  (dolist (it *actions4*)
    (ematch it
      ((list :action name :parameters params :precondition pre :effect eff)
       (let ((&pre (&nnf-dnf pre))
             (&eff (&nnf-dnf/effect eff)))
         (funcall &pre
                  (lambda (pre)
                    (funcall &eff
                             (lambda (eff)
                               (push (list :action name :parameters params
                                           :precondition pre
                                           :effect eff)
                                     *actions5*))))))))))

(defun remove-disjunction-axioms ()
  (dolist (it *axioms*)
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
                  (error "removing exist from conditional effects / fluents not allowed here"))

                 (_
                  (push condition body-acc)))))
      (rec condition)
      `(exists ,args-acc (and ,@body-acc)))))


(progn
  (print (move-exists/condition `(exists (a b c) (three a b c))))
  (print (move-exists/condition `(and
                                  (p1)
                                  (exists (a) (p2 a))
                                  (exists (a) (p3 a)))))
  (print (move-exists/condition `(and
                                  (p1)
                                  (exists (a)
                                          (and (and (p3 a)
                                                    (p4 a))
                                               (exists (b) (p2 a b))))))))

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

(progn
  (print (move-exists/effect `(and (p1)
                                   (and (p1) (p1))
                                   (when (exists (a) (clear a))
                                     (and (p2) (and (p2) (and (p2) (p2)))))))))

(defun move-exists-actions ()
  (dolist (it *actions5*)
    (ematch it
      ((list :action name :parameters params :precondition pre :effect eff)
       (push 
        (match (move-exists/condition pre)
          ;; remove exists
          (`(exists ,args ,condition)
            (list :action name
                  :parameters (append params args)
                  :original-parameters params
                  :precondition condition
                  :effect (move-exists/effect eff)))
          (condition
           (list :action name
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
      ((list :action name :parameters params :original-parameters oparams :precondition pre :effect eff)
       (push
        (list :action name :parameters params :original-parameters oparams :precondition pre :effect (simplify-effect eff))
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

(print (simplify-effect `(clear)))

(print (simplify-effect `(when (and) (and (a) (b)))))

(print (simplify-effect `(when (and) (forall () (and (a) (b))))))

;;; parse8 --- output

(defun parse8 ()
  (list :type *types*
        :objects *objects*
        :predicates *predicates*
        :init *init4*
        :goal *goal4*
        :axioms *axioms6*
        :actions *actions7*))

