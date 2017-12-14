
;;; Invariant synthesys

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;;; initial candidates

(define-constant +counted-variable+ :?counted
  :documentation "
All candidates are supposed to have a single counted variable.
Thus it is ok to assume a specific name for the variable.
Equality-wise, it never conflicts normal variables because they are always interned in package PDDL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (candidate (:constructor candidate (parameters atoms)))
    parameters
    atoms))

(defun initial-candidates ()
  (iter outer
        (for p in (modifiable-fluent-predicates))
        (ematch p
          ((list* name parameters)
           (let ((parameters (make-gensym-list (length parameters) "?V")))
             ;; without counted variable
             (collecting (candidate parameters (list `(,name ,@parameters))))
             ;; with a single counted variable
             (iter (for i below (length parameters))
                   (for i-params =
                        (iter (for var in parameters)
                              (for j below (length parameters))
                              (when (/= i j)
                                (collect var))))
                   (for args =
                        (iter (for var in parameters)
                              (for j below (length parameters))
                              (collect
                                  (if (= i j)
                                      +counted-variable+
                                      var))))
                   (in outer 
                       (collecting (candidate i-params
                                              `((,name ,@args)))))))))))

#+(or)
(strips:with-parsed-information (strips:parse (strips:%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  (strips::initial-candidates))

#+(or)
(strips:with-parsed-information (strips:parse (strips:%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (strips::initial-candidates))

;;; finding invariance

(defun find-invariants ()
  (let ((open (initial-candidates))
        invariants)
    (restart-bind
        ((new-candidate
          (lambda (candidates)
            (appendf open candidates))))
      (iter (for candidate = (pop open))
            (while candidate)
            (when (prove-invariant candidate)
              (push candidate invariants))))
    invariants))

;;; proving invariance

(defun prove-invariant (candidate)
  (ematch candidate
    ((candidate atoms)
     (iter (for a in *actions*)
           (when (or (too-heavy-p a atoms)
                     #+(or)
                     (unbalanced-p a atoms))
             (return-from prove-invariant nil)))
     t)))

#+(or)
(strips::with-parsed-information (strips:parse (strips:%rel "ipc2011-opt/transport-opt11/p01.pddl"))
  (remove-if-not #'strips::prove-invariant
                 (strips::initial-candidates)))

;;; too-heavy-p
;; terminology:
;; i-params : parameters of an invarinat candidate
;; i-atoms    : atomic formulas in an invarinat candidate
;; a-params : action parameters
;; q-params : quantified parameters in the effects

(defun delete-effect-p (effect)
  (match effect
    (`(forall ,_ (when ,_ (not ,_))) t)))

(defun too-heavy-p (action i-atoms)
  (labels ((duplicate-quantified-effect (names add)
           "duplicate and assign unique params to non-trivially quantified effects"
           (iter (for e in add)
                 (match e
                   (`(forall ,params ,(and body `(when ,_ (,name ,@_))))
                     (when (member name names)
                       (flet ((rename (unique)
                                `(forall ,unique
                                         ,(iter (with body = body)
                                                (for p in params)
                                                (for u in unique)
                                                (setf body (subst u p body))
                                                (finally (return body))))))
                         ;; duplicate
                         (collecting (rename (make-gensym-list (length params) "?")))
                         (when params
                           (collecting (rename (make-gensym-list (length params) "?")))))))))))
    ;; main body
    (ematch action
      ((plist :precondition `(and ,@precond) :effect `(and ,@effects))
       (let* ((names (mapcar #'first i-atoms))
              (add   (remove-if #'delete-effect-p effects))
              (add+  (duplicate-quantified-effect names add)))
         (when (>= (length add+) 2)
           (map-combinations (lambda (effects-pair)
                               (when (multiple-value-call
                                         #'satisfiable
                                       (too-heavy-constraints i-atoms precond effects-pair))
                                 (return-from too-heavy-p t)))
                             add+ :length 2)))
       nil))))

(defun ignore-negation (atom)
  (match atom
    (`(not ,x) x)
    (_ atom)))

(defun not-equal (atom1 atom2)
  (ematch* (atom1 atom2)
    ((`(,head1 ,@args1) `(,head2 ,@args2))
     (when (eq head1 head2)
       (remove-if (lambda-match ((cons x (eq x)) t))
                  (mapcar #'cons args1 args2))))))

(defun cover (atom i-atoms)
  (ematch atom
    (`(,head ,@args)
      (list ; there is no disjunction
       (iter (with covered = (find head i-atoms :key #'first))
             (for x in args)
             (for y in (cdr covered))
             (unless (eq y +counted-variable+)
               (collecting (cons x y))))))))

(defun conjunction (conditions)
  (let (pos neg aliases inequality)
    (iter (for condition in conditions)
          (match condition
            (`(not (= ,x ,y)) (push (list (cons x y))        inequality))
            (`(= ,x ,y)       (push (list (list (cons x y))) aliases))
            (`(not ,x)        (push x neg))
            (_                (push condition pos))))
    (iter (for p in pos)
          (iter (for n in neg)
                (push (not-equal p n) inequality)))
    (values aliases inequality)))

(defun too-heavy-constraints (i-atoms precond effects-pair)
  (match effects-pair
    ((list `(forall ,_ (when (and ,@conditions1) ,atom1))
           `(forall ,_ (when (and ,@conditions2) ,atom2)))
     (let ((atom1+ (ignore-negation atom1))
           (atom2+ (ignore-negation atom2))
           ;;  Aliases = conjunctions of equality. (x1 = y1 and x2 = y2 ... )
           ;; originally called Assignment in the python code.
           ;; 
           ;;  Inequality = disjunctions of inequality. (x1 != y1 or x2 != y2 ...)
           ;; originally NegativeClause in constraints.py.
           ;;
           ;;  Now what is more complicated is "combinatorial_assignments" in the python code.
           ;; This is a conjunctions of disjunctions of conjunctions of equality.
           ;; Thus when computing the correct aliases, we should enumerate all combinations.
           ;; 
           ;;  Aliases is thus a list of lists of alists.
           ;; Inequality is a list of alists (conjunctions of equalities).
           (aliases nil)
           (inequality nil))
       ;; ensure_inequality
       (push (not-equal atom1+ atom2+) inequality)
       ;; ensure_cover: this assumes all atoms in an invariant have the different names
       (push (cover atom1+ i-atoms) aliases)
       (push (cover atom2+ i-atoms) aliases)
       ;; ensure_conjunction_sat
       (multiple-value-bind (more-aliases more-inequality)
           (conjunction (append precond conditions1 conditions2
                                (list (negate atom1) (negate atom2))))
         (appendf aliases more-aliases)
         (appendf inequality more-inequality))
       (values aliases inequality)))))

(print-values
  (too-heavy-constraints '((at ?thing :?counted))
                         '((at ?x ?l1) (at ?x ?l2))
                         '((forall () (when (and) (at ?x ?l3)))
                           (forall () (when (and) (at ?x ?l4))))))

(print (too-heavy-p '(:precondition (and (at ?x ?l1) (at ?x ?l2))
                      :effect (and
                               (forall () (when (and) (at ?x ?l3)))
                               (forall () (when (and) (at ?x ?l4)))
                               (forall () (when (and) (not (at ?x ?l1))))
                               (forall () (when (and) (not (at ?x ?l2))))))
                    '((at ?thing :?counted))))

(print-values
  (too-heavy-constraints '((at ?thing :?counted))
                         '((at ?x ?l1) (at ?x ?l2) (not (= ?l1 ?l2)))
                         '((forall () (when (and) (at ?x ?l3)))
                           (forall () (when (and) (at ?x ?l4))))))

(print (too-heavy-p '(:precondition (and (at ?x ?l1) (at ?x ?l2) (not (= ?l1 ?l2)))
                      :effect (and
                               (forall () (when (and) (at ?x ?l3)))
                               (forall () (when (and) (at ?x ?l4)))
                               (forall () (when (and) (not (at ?x ?l1))))
                               (forall () (when (and) (not (at ?x ?l2))))))
                    '((at ?thing :?counted))))

(progn ;; for hideshow minor mode
  #+(or)
  (too-heavy-p '(:precondition (and (at ?x ?l1) (at ?x ?l2))
                 :effect (and
                          (forall () (when (and) (at ?x ?l3)))
                          (forall () (when (and) (at ?x ?l4)))
                          (forall () (when (and) (not (at ?x ?l1))))
                          (forall () (when (and) (not (at ?x ?l2))))))
               '((at ?thing :?counted)))

  ;; trying to make too-heavy-constraints more understantable

  (defun too-heavy-constraints-sexp (i-atoms precond effects-pair)
    "When these constrants are satisfied, 
the effect may increase the number of true atom in i-atoms by more than two"
    (match effects-pair
           ((list `(forall ,_ (when (and ,@conditions1) ,atom1))
                  `(forall ,_ (when (and ,@conditions2) ,atom2)))
            (ematch* ((ignore-negation atom1) (ignore-negation atom2))
                     ((`(,head1 ,@args1) `(,head2 ,@args2))
                      (flet ((==  (x y) `(== ,x ,y)) ; names chosen because they don't conflict CL symbols
                             (!= (x y) `(!= ,x ,y)))
                        `(and
                          ;; ensure_inequality
                          ,@(when (eq head1 head2)
                              `((or ,@(mapcar #'!= args1 args2))))

                          ;; ensure_cover: this assumes all atoms in an invariant have the different names
                          ,@(iter (with covered = (find head1 i-atoms :key #'first))
                                  (for x in args1)
                                  (for y in (cdr covered))
                                  (unless (eq y +counted-variable+)
                                    (collect (== x y))))
                          ,@(iter (with covered = (find head2 i-atoms :key #'first))
                                  (for x in args2)
                                  (for y in (cdr covered))
                                  (unless (eq y +counted-variable+)
                                    (collect (== x y))))
                          
                          ;; ensure_conjunction_sat
                          ,@(let (pos neg acc)
                              (iter (for condition in (append precond conditions1 conditions2
                                                              (list (negate atom1) (negate atom2))))
                                    (match condition
                                           (`(not (= ,x ,y)) (push (!= x y) acc))
                                           (`(= ,x ,y)       (push (== x y) acc))
                                           (`(not ,x)        (push x neg))
                                           (_                (push condition pos))))
                              
                              (iter (for p in pos)
                                    (iter (for n in neg)
                                          (ematch* (p n)
                                                   ((`(,head1 ,@args1) `(,head2 ,@args2))
                                                    (when (eq head1 head2)
                                                      (push
                                                       `(or ,@(mapcar #'!= args1 args2)) acc))))))
                              acc))))))))

  ;; considering the second case below

  #+(or)
  #S(CANDIDATE :PARAMETERS (#:?V1532) :ATOMS ((PDDL::AT :?COUNTED #:?V1532)))
  #+(or)
  #S(CANDIDATE :PARAMETERS (#:?V1531) :ATOMS ((PDDL::AT #:?V1531 :?COUNTED)))

  #+(or)
  (too-heavy-constraints-sexp '((at ?thing :?counted))
                              '((at ?x ?l1) (at ?x ?l2))
                              '((forall nil (when (and) (at ?x ?l3)))   ; implies (not (at ?x ?l3)) before application
                                (forall nil (when (and) (at ?x ?l4))))) ; implies (not (at ?x ?l4)) before application

  #+(or)
  (AND (OR (!= ?X ?X) (!= ?L3 ?L4))
       (== ?L3 :?COUNTED)
       (== ?L4 :?COUNTED)
       (OR (!= ?X ?X) (!= ?L1 ?L3))
       (OR (!= ?X ?X) (!= ?L1 ?L4))
       (OR (!= ?X ?X) (!= ?L2 ?L3))
       (OR (!= ?X ?X) (!= ?L2 ?L4)))

  ;; if we ignore the obvious and the duplicates
  #+(or)
  (AND (!= ?L3 ?L4)
       (== ?X ?THING)
       (== ?L3 :?COUNTED)
       (== ?L4 :?COUNTED)
       (!= ?L1 ?L3)
       (!= ?L1 ?L4)
       (!= ?L2 ?L3)
       (!= ?L2 ?L4))

  ;; (!= ?L3 ?L4) and (== ?L3 :?COUNTED) and (== ?L4 :?COUNTED) is a contradiction.
  )

;;; unbalanced-p

(defun unbalanced-p (action i-atoms)
  (ematch action
    ((plist :preconditions `(and ,@precond) :effects `(and ,@effects))
     (let* ((names (mapcar #'first i-atoms))
            (rels (iter (for e in effects)
                        (match e
                          (`(forall ,_ (when ,_ (,name ,@_)))
                            (when (member name names)
                              (collecting e))))))
            (adds   (remove-if #'delete-effect-p rels))
            (dels   (remove-if-not #'delete-effect-p rels)))
       (iter outer
             (for add in adds)
             (for (values aliases inequality) = (minimal-renamings i-atoms action add))
             (when (iter (for del in dels)
                         (always (unbalanced-effects-p aliases inequality precond add del i-atoms)))
               (refine-candidate add dels)
               (return-from unbalanced-p t)))))))

(defun minimal-renamings (i-atoms action add)
  (let (aliases inequality)
    (match add
      (`(forall ,_ (when (and ,@_) ,atom))
        ;; since the renaming should be minimal,
        ;; disallow aliasing each parameter to a single group.
        ;; without this constraint, every parameter can be aliased to a single entity.
        (push (cover atom i-atoms) aliases)
        (let ((ec (make-equivalence aliases)))
          (ematch action
            ((plist :parameters params)
             (iter (for (p1 . rest) on params)
                   (iter (for p2 in rest)
                         (when (/= (class-id ec p1) (class-id ec p2))
                           (push (cons p1 p2) inequality))))))
          (values aliases inequality))))))

(print-values
  (minimal-renamings '((at ?thing :?counted))
                     '(:action move
                       :parameters (?x ?l1 ?l2)
                       :precondition (and (at ?x ?l1))
                       :effect (and
                                (forall () (when (and) (at ?x ?l2)))
                                (forall () (when (and) (not (at ?x ?l1))))))
                     '(forall () (when (and) (at ?x ?l2)))))

#+(or)
(print (unbalanced-p '(:action move
                       :parameters (?x ?l1 ?l2)
                       :precondition (and (at ?x ?l1))
                       :effect (and
                                (forall () (when (and) (at ?x ?l2)))
                                (forall () (when (and) (not (at ?x ?l1))))))
                     '((at ?thing :?counted))))

(defun unbalanced-effects-p (aliases1 inequality1 precond add del i-atoms)
  (ematch* (add del)
    ((`(forall ,_ (when (and ,@conditions1) ,pos))
      `(forall ,_ (when (and ,@conditions2) (not ,neg))))
     (let ((aliases2 (list (cover neg i-atoms)))    ; e.atom /= e′.atom
           (inequality2 (list (not-equal add del))) ; covers(V , Φ, e′ .atom)
           )
       ;; aliases1/inequality1 are from the operator params and the add effect
       ;;
       ;; "the quantified variables of e′ can be renamed so that"....
       (iter (with check-constants = nil)
             (with aliases3 = nil)
             (with inequality3 = nil)
             ;; --> From python comment: "Since we may only rename the quantified
             ;; variables of the delete effect we need to check that "renamings" of
             ;; constants are already implied by the unbalanced_renaming (of the of
             ;; the operator parameters). The following system is used as a helper
             ;; for this. It builds a conjunction that formulates that the constants
             ;; are NOT renamed accordingly. We below check that this is impossible
             ;; with each unbalanced renaming."
             (for (x y . rest) on (compute-mapping (make-equivalence aliases2)))
             (when (not (variablep y)) ; constant
               (setf check-constants t)
               (push (list (cons x y)) inequality3))
             (finally                   ; not necessarily the FINALLY clause, but this makes the connection clear
              (when check-constants
                (when (satisfiable (append aliases1 aliases3) (append inequality1 inequality3))
                  ;; From python comment: "it is possible that the operator arguments
                  ;; are not mapped to constants as required for covering the delete
                  ;; effect"
                  (return-from unbalanced-effects-p (values aliases1 inequality1))))))
       (let (;; new_sys in python
             (aliases4 (append aliases1 aliases2))
             (inequality4 (append inequality1 inequality2)))
         ;; (o′.precond ∧ e.cond ∧ ¬e.atom) |= (e′.cond ∧ e′ .atom)  (f**king operator precedence!)
         (multiple-value-bind (lhs-aliases lhs-inequality)
             (conjunction (append precond conditions1 (list (negate pos)))) ; (o′.precond ∧ e.cond ∧ ¬e.atom)
           (when (satisfiable (append aliases1 lhs-aliases)
                              (append inequality1 lhs-inequality))
             ;; implies_del_effect in python, (e′.cond ∧ e′ .atom)
             (iter (for c1 in (list* neg conditions2))
                   (for possible-assignments =
                        (iter (for c2 in (append precond conditions1 (list (negate pos))))
                              (match* (c1 c2)
                                ((`(not (,name ,@args1))
                                  `(not (,(eq name) ,@args2)))
                                 (collecting (mapcar #'cons args1 args2)))
                                ((`(,(and name (not 'not)) ,args1)
                                  `(,(eq name) ,args2))
                                 (collecting (mapcar #'cons args1 args2))))))
                   (when (null possible-assignments)
                     (return-from unbalanced-effects-p (values aliases1 inequality1)))
                   (collecting possible-assignments into aliases5)
                   (finally
                    (appendf aliases4 aliases5)))))
         (unless (satisfiable aliases4 inequality4)
           (return-from unbalanced-effects-p (values aliases1 inequality1))))))))

(defun refine-candidates ()
  )
         
;; couldnt understand what they are doing!
