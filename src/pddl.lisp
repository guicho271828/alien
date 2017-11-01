;;; parser

(in-package :strips)

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
                          ((cons type _) `(,type o)))
                        parsed))
       (dolist (p parsed)
         (match p
           ((cons self (and parent (not 'object)))
            (push `((,parent ?o) (,self ?o)) *axioms*))))))))

(defun grovel-constants (domain)
  (ematch domain
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
       (ematch predicate
         ((list* name typed-def)
          (let* ((parsed (parse-typed-def typed-def))
                 (w/o-type `(,name ,@(mapcar #'car parsed))))
            (push w/o-type *predicates*)
            (dolist (pair parsed)
              (match pair
                ((cons arg (and type (not 'object)))
                 (push `((,type ,arg) ,w/o-type) *axioms*)))))))))))

(defun grovel-init (problem)
  (ematch problem
    ((assoc :init predicates)
     (dolist (condition predicates)
       (ematch condition
         ((list* '= _)
          (format t "~&; skipping ~a" condition))
         (_
          (setf *predicates* (flatten-types condition))))))))

(defun grovel-goal (problem)
  (ematch problem
    ((assoc :goal condition)
     (setf *goal* (flatten-types condition)))))

(defun grovel-actions (domain)
  (dolist (it (find :action domain :key #'first))
    (ematch it
      ((list :action name :parameters params
             :precondition pre :effects eff)
       (let* ((parsed (parse-typed-def params))
              (w/o-type (mapcar #'car parsed))
              (type-conditions (mapcar (lambda-ematch ((cons arg type) `(,type ,arg))) parsed)))
         (push `(:action ,name
                         :parameters ,w/o-type
                         :precondition
                         (and ,@type-conditions
                              ,(flatten-types pre))
                         :effects ,eff)
               *actions*))))))

(defun grovel-axioms (domain)
  (dolist (it (find :derived domain :key #'first))
    (push 
     (ematch it
       ((list :derived derived condition)
        (list :derived derived (flatten-types conditon))))
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
    ((list 'not (and a (list* (or 'and 'or 'forall 'exists 'not) _)))
     (to-nnf (negate a)))
    ((list 'not _)
     condition)
    ((list 'imply a b)
     `(or ,(to-nnf `(not ,a))
          ,(to-nnf b)))
    ((list* (and kind (or 'and 'or)) rest)
     `(,kind ,@(mapcar #'to-nnf rest)))
    ((list (and kind (or 'forall 'exist)) args body)
     `(,kind ,args ,(to-nnf body)))
    ((list 'when condition body)
     `(when ,(to-nnf condition)
        ,(to-nnf body)))))

(defun nnf-actions ()
  (dolist (it *actions*)
    (push 
     (ematch it
       ((list :action name :parameters params :precondition pre :effects eff)
        (list :action name :parameters params
              :precondition (to-nnf pre)
              :effects (to-nnf eff))))
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
         (push `(,a ,e) *axioms4*)
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
       ((list :action name :parameters params :precondition pre :effects eff)
        (list :action name :parameters params
              :precondition (remove-forall/condition pre)
              :effects (remove-forall/effect eff))))
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
     (push `((,goal-axiom) ,(remove-forall/condition *goal*)) *axioms4*)
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

(defun &NNF-DNF (condition)
  ;; now we have only and, or, exist, not, predicates.
  ;; OR clause is converted into an iterator.
  (ematch condition
    (`(or ,@rest)
      (let ((restk (mapcar #'&NNF-DNF rest)))
        (lambda (k)
          ;; calls k for each element
          (dolist (nowk restk)
            (funcall nowk k)))))

    (`(and ,first ,@rest)
     (let ((firstk (&NNF-DNF first))
           (restk  (&NNF-DNF `(and ,@rest))))
       (lambda (k)
         (funcall firstk
                  (lambda (result)
                    (funcall restk
                             (lambda (result2)
                               (funcall k (cons result result2)))))))))
    ((list 'and)
      (lambda (k)
        (funcall k nil)))

    (`(exists ,args ,body)
      (let ((bodyk (&NNF-DNF body)))
        (lambda (k)
          (funcall bodyk
                   (lambda (result)
                     (funcall k `(exists ,args ,result)))))))
    
    (_
     (lambda (k) (funcall k condition)))))

(defun NNF-DNF (condition)
  (let (acc)
    (funcall (&NNF-DNF condition)
             (lambda (result)
               (push result acc)))
    (print acc)))

(NNF-DNF 'a)
(NNF-DNF '(or a b))
(NNF-DNF '(or a b c (or d e)))
(NNF-DNF '(and x (or a b)))
(NNF-DNF '(and (or x y) (or a b)))
(NNF-DNF '(and (or x y) c (or a b)))
(NNF-DNF '(and (or (and x z) y) c (or a b)))
(NNF-DNF '(or (and x (or w z)) y))

(NNF-DNF '(and (clear ?x)
           (or (clear ?w)
            (not (clear ?z)))))

(NNF-DNF '(exists (?x ?y)
           (or (clear ?x)
            (not (clear ?y)))))

;; (defun remove-disjunction-actions ()
;;   (dolist (it *actions3*)
;;     (push 
;;      (ematch it
;;        ((list :action name :parameters params :precondition pre :effects eff)
;;         (list :action name :parameters params
;;               :precondition 
;;               :effects (NNF-DNF eff))))
;;      *actions5*)))
;; 
;; (defun remove-disjunction-axioms ()
;;   (dolist (it *axioms*)
;;     (push 
;;      (ematch it
;;        ((list :derived derived condition)
;;         (list :derived derived (remove-forall/condition condition))))
;;      *axioms5*)))

