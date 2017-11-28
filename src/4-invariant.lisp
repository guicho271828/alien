
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

;;; proving invariance

(defun prove-invariant (candidate)
  (ematch candidate
    ((candidate parameters atoms)
     (iter (for a in *actions*)
           (when (or (too-heavy-p a parameters atoms)
                     (unbalanced-p a parameters atoms))
             (return-from prove-invariant nil)))
     t)))


(defun delete-effect-p (effect)
  (match effect
    (`(forall ,_ (when ,_ (not ,_))) t)))

(defun too-heavy-p (action i-params i-atoms)
  ;; terminology:
  ;; i-params : parameters of an invarinat candidate
  ;; i-atoms    : atomic formulas in an invarinat candidate
  ;; a-params : action parameters
  ;; q-params : quantified parameters in the effects
  (ematch action
    ((plist :parameters a-params :preconditions `(and ,@precond) :effects `(and ,@effects))
     (let* ((names (mapcar #'first i-atoms))
            (add   (remove-if #'delete-effect-p effects))
            ;; duplicate and assign unique params to non-trivially quantified effects
            (add+  (iter (for e in add)
                         (match e
                           (`(forall () (when ,_ (,name ,@_)))
                             (when (member name names)
                               ;; 'NOT is not part of NAMES; should be ok.
                               (collecting e)))
                           (`(forall ,params ,(and body `(when ,_ (,name ,@_))))
                             (when (member name names)
                               ;; 'NOT is not part of NAMES; should be ok.
                               (flet ((rename (unique)
                                        `(forall ,unique
                                                 ,(iter (with body = body)
                                                        (for p in params)
                                                        (for u in unique)
                                                        (setf body (subst u p body))
                                                        (finally (return body))))))
                                 ;; duplicate
                                 (collecting (rename (make-gensym-list (length params) "?")))
                                 (collecting (rename (make-gensym-list (length params) "?"))))))))))
       (map-combinations (lambda (effects-pair)
                           (when (unify i-params i-atoms a-params precond effects-pair)
                             (return-from too-heavy-p t)))
                         add+ :length 2))
     nil)))

(defun ignore-negation (atom)
  (match atom
    (`(not ,x) x)
    (_ atom)))

(defun unify (i-params a-params i-atoms precond effects-pair)
  (match effects-pair
    ((list `(forall ,q-params1 (when (and ,@conditions1) ,atom1))
           `(forall ,q-params2 (when (and ,@conditions2) ,atom2)))
     (let ((atom1 (ignore-negation atom1))
           (atom2 (ignore-negation atom2))
           (bound (append q-params1 q-params2 a-params i-params))
           ;; 
           ;; note: there should be a sinlge free variable and other atoms in
           ;;       the invariants should share it
           (free (set-difference (rest (first i-atoms)) i-params))

           ;; assignments = conjunctions of equality.
           ;; (x1 = y1 and x2 = y2 and ... )
           ;; originally called Assignment in the python code.
           ;; we follow the unification/prolog term, aliasing.
           ;; alias-list is an alist.
           (aliases nil)
           ;;
           ;; inequality constraints: disallowed assignments.
           ;; disjunctions of inequality = negation of aliases.
           ;; (x1 != y1 or x2 != y2 ...) = not (x1 = y1 and x2 = y2 ...)
           ;; originally NegativeClause in constraints.py.
           ;; it is no longer associative.
           ;; thus this is a list of alists.
           (inequality nil))
       (assert (= 1 (length free)))
       ;; 
       ;; ensure_inequality in python code
       (match* (atom1 atom2)
         ((`(,head1 ,@args1) `(,(eq head1) ,@args2))
          ;; when two heads are the same, the corresponding arguments should be all different
          (appendf inequality (mapcar #'cons args1 args2))))

       ;; ensure_cover 1
       (iter (for atom in i-atoms)
             
             )
       
       ;; ensure_cover 2
       (iter (for var in bound)
             (push (cons var free) inequality))
       
       (when (and (covers i-params atoms atom1)
                  (covers i-params atoms atom2)
                  precond
                  condition1
                  condition2)
         (return-from too-heavy-p t))))))

;; (defun covers (parameters atoms atom)
;;   )
;; 
;; (defun unbalanced-p (candidate parameters atoms)
;;   (ematch candidate
;;     ((candidate parameters atoms)
;;      (iter (for a in *actions*)
;;            (ematch a
;;              ((plist :parameters action-params :effect `(and ,@effects))
;;               (iter (for e in effects)
;;                     (match e
;;                       (`(forall ,_ (when ,_ (,(eq name) ,@_)))
;;                         
;;                         (return-from unbalanced-p t))
;;                       (`(forall ,_ (when ,_ (not (,(eq name) ,@_))))
;;                         (return-from unbalanced-p t))))))))))
