
;;; Invariant synthesys

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defmacro with-parsed-information (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((list :type *types*
            :objects *objects*
            :predicates *predicates*
            :init *init*
            :goal *goal*
            :axioms *axioms*
            :actions *actions*)
      ,@body)))

;; Usage:
#+(or)
(strips:with-parsed-information (strips:parse (asdf:system-relative-pathname :strips "axiom-domains/opttel-adl-derived/p01.pddl"))
  strips:*types*)

(defun preprocess (info)
  (with-parsed-information info
    (list* :invarinants (find-invariants)
            info)))

;;; modifiable-fluent-predicates

(defun modifiable-fluent-predicates ()
  "Sec 5.1 Initial candidates par.2. Excludes constant and derived predicates"
  (remove-if (disjoin #'constant-predicate-p
                      #'derived-predicate-p)
             *predicates*))

(defun fluent-predicate-p (predicate)
  (ematch predicate
    ((list* name _)
     (iter (for a in *actions*)
           (ematch a
             ((plist :effect `(and ,@effects))
              (iter (for e in effects)
                    (match e
                      (`(forall ,_ (when ,_ (,(eq name) ,@_)))
                        (return-from fluent-predicate-p t))
                      (`(forall ,_ (when ,_ (not (,(eq name) ,@_))))
                        (return-from fluent-predicate-p t))))))))))

(defun constant-predicate-p (predicate)
  (not (fluent-predicate-p predicate)))

(defun derived-predicate-p (predicate)
  (ematch predicate
    ((list* name _)
     (iter (for a in *axioms*)
           (match a
             (`(:derived (,(eq name) ,@_) ,@_)
               (return-from derived-predicate-p t)))))))
  
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
(strips:with-parsed-information (strips:parse (asdf:system-relative-pathname :strips "axiom-domains/opttel-adl-derived/p01.pddl"))
  (strips::initial-candidates))

;;; finding invariance

(define-condition new-candidate ()
  ((candidates :reader new-candidate-candidates
               :initarg :candidates)))

(defun find-invariants ()
  (let ((open (initial-candidates))
        invariants)
    (handler-bind
        ((new-candidate
          (lambda (c)
            (appendf open (new-candidate-candidates c)))))
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
                     (unbalanced-p a atoms))
             (return-from prove-invariant nil)))
     t)))

;;; too-heavy-p
;; terminology:
;; i-params : parameters of an invarinat candidate
;; i-atoms    : atomic formulas in an invarinat candidate
;; a-params : action parameters
;; q-params : quantified parameters in the effects

(defun delete-effect-p (effect)
  (match effect
    (`(forall ,_ (when ,_ (not ,_))) t)))

(defun duplicate-quantified-effect (names add)
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
                  (collecting (rename (make-gensym-list (length params) "?"))))))))))

(defun too-heavy-p (action i-atoms)
  (ematch action
    ((plist :preconditions `(and ,@precond) :effects `(and ,@effects))
     (let* ((names (mapcar #'first i-atoms))
            (add   (remove-if #'delete-effect-p effects))
            ;; duplicate and assign unique params to non-trivially quantified effects
            (add+  (duplicate-quantified-effect names add)))
       (map-combinations (lambda (effects-pair)
                           (when (multiple-value-call
                                     #'satisfiable
                                   (too-heavy-constraints i-atoms precond effects-pair))
                             (return-from too-heavy-p t)))
                         add+ :length 2))
     nil)))

(defun ignore-negation (atom)
  (match atom
    (`(not ,x) x)
    (_ atom)))

(defun too-heavy-constraints (i-atoms precond effects-pair)
  (match effects-pair
    ((list `(forall ,_ (when (and ,@conditions1) ,atom1))
           `(forall ,_ (when (and ,@conditions2) ,atom2)))
     (let ((atom1+ (ignore-negation atom1))
           (atom2+ (ignore-negation atom2))
           ;; Aliases = conjunctions of equality. (x1 = y1 and x2 = y2 ... )
           ;; originally called Assignment in the python code. (We follow the
           ;; unification/prolog terminology.)
           ;; 
           ;; Inequality = disjunctions of inequality. (x1 != y1 or x2 != y2 ...)
           ;; originally NegativeClause in constraints.py.
           ;;
           ;; Now what is more complicated is "combinatorial_assignments" in the python code.
           ;; This is a conjunctions of disjunctions of conjunctions of equality.
           ;; Thus when computing the correct aliases, we should enumerate all combinations.
           ;; 
           ;; Aliases is thus a list of lists of alists.
           ;; Inequality is a list of alists.
           (aliases nil)
           (inequality nil))
       ;; 
       (ematch* (atom1+ atom2+)
         ((`(,head1 ,@args1) `(,head2 ,@args2))
          
          ;; ensure_inequality
          (when (eq head1 head2)
            (push (mapcar #'cons args1 args2) inequality))

          ;; ensure_cover: this assumes all atoms in an invariant have the different names
          (let ((covered (find head1 i-atoms :key #'first)))
            (push (list (mapcar #'cons args1 (cdr covered))) aliases))
          (let ((covered (find head2 i-atoms :key #'first)))
            (push (list (mapcar #'cons args2 (cdr covered))) aliases))

          ;; ensure_conjunction_sat
          (let (pos neg)
            (iter (for condition in (append precond conditions1 conditions2
                                            (list (negate atom1) (negate atom2))))
                  (match condition
                    (`(not (= ,x ,y)) (push (list (cons x y))         inequality))
                    (`(= ,x ,y)       (push (list (list (cons x y))) aliases))
                    (`(not ,x)        (push x neg))
                    (_                (push condition pos))))
            (iter (for p in pos)
                  (iter (for n in neg)
                        (ematch* (p n)
                          ((`(,head1 ,@args1) `(,head2 ,@args2))
                           (when (eq head1 head2)
                             (push (mapcar #'cons args1 args2) inequality)))))))
          (values aliases inequality)))))))

;;; satisfiability

(defun satisfiable (aliases inequality)
  (apply #'map-product
         (lambda (&rest single-aliases)
           (when (test-aliases (reduce #'append single-aliases) inequality)
             (return-from satisfiable t)))
         aliases))


(defun test-aliases (aliases inequality)
  (let* ((elems nil)
         (elem-class nil)
         (class-elems nil)
         (mapping nil))
    ;; the implementation here is entirely based on a list and slow
    ;; should use a more efficient datastructure
    (iter (for (x . y) in aliases)
          (pushnew x elems)
          (pushnew y elems))
    (iter (for e in elems)
          (for i from 0)
          (setf (getf elem-class e) i)
          (setf (getf class-elems i) (list e)))
    ;; merge the classes
    (iter (for (x . y) in aliases)
          (for xc = (getf elem-class x))
          (for yc = (getf elem-class y))
          (when (> xc yc)
            (rotatef x y)
            (rotatef xc yc))
          ;; now xc < yc
          (setf (getf elem-class y) xc)
          (appendf (getf class-elems xc) (getf class-elems yc))
          (remf class-elems yc))
    ;; find the representative
    (iter (for (class elems . rest) on class-elems)
          (for constant = nil)
          (for variables = nil)
          (dolist (e elems)
            (if (variablep e)
                (push e variables)
                (if constant ;; binding the same variable to two constants
                    (return-from test-aliases nil)
                    (setf constant e))))
          (dolist (v variables)
            (setf (getf mapping v) constant)))
    ;; check if for all disjunctions, at least one clause is satisfied
    (every (lambda (disjunction)
             (some (lambda-ematch
                     ((cons x y)
                      (not (eq (getf mapping x)
                               (getf mapping y)))))
                   disjunction))
           inequality)))

;;; unbalanced-p

(defun unbalanced-p (action i-atoms)
  (ematch action
    ((plist :preconditions `(and ,@precond) :effects `(and ,@effects))
     (let* ((names (mapcar #'first i-atoms))
            (rels (iter (for e in effects)
                       (match e
                         (`(forall ,_ (when ,_ (,name ,@_)))
                           (when (member name names)
                             (colllecting e))))))
            (adds   (remove-if #'delete-effect-p rels))
            (dels   (remove-if-not #'delete-effect-p rels)))
       (iter (for add in adds)
             (when (unbalanced-add-effect-p i-atoms action add dels)
               (return-from unbalanced-p t)))
       nil))))

(defun unbalanced-add-effect-p (i-atoms action add dels)
  (let (aliases inequality)
    
  (let ((minimal-renamings
         (minimal-renamings action add)))
    ))

(defun minimal-renamings (i-atoms action add)
  (let (aliases inequality)
    (match add
      (`(,head1 ,@args1)
        (let ((covered (find head1 i-atoms :key #'first)))
          (push (list (mapcar #'cons args1 (cdr covered))) aliases))
    
    
  
