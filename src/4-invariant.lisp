
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (candidate (:constructor candidate (parameters atoms)))
    parameters
    atoms))

(defun initial-candidates ()
  (iter outer
        (for p in (modifiable-fluent-predicates))
        (ematch p
          ((list* _ parameters)
           (collecting (candidate parameters (list p)))
           (iter (with parameters = (copy-list parameters))
                 (repeat (length parameters))
                 (in outer 
                     (collecting (candidate (copy-list (rest parameters)) (list p))))
                 ;; caution: destructively modifies PARAMETERS
                 (setf parameters (rotate parameters)))))))

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

#|

Now, this part is extremely unbiguous in the original document
as well as in the source code of translate.py.

The crucial problem in the AIJ09 paper is that "satisfiable" and "renamed" is not defined.

First, "renamed": I assume this means that 

|#


(defun too-heavy-p (action parameters atoms)
  (ematch action
    ((plist :parameters a-params :preconditions `(and ,@precond) :effects `(and ,@effects))
     (let ((names (mapcar #'first atoms))
           (add (remove-if #'delete-effect-p effects)))
       ;; duplicate and assign unique params to non-trivially quantified effects
       (let ((add+ (iter (for e in add)
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
         ;; 
         ;; note that the precondition could contain negative ones
         ;; there is no universal preconditions (they are moved to derived predicates)
         ;; 
         (iter (for (e1 . rest) on add+)
               (iter (for e2 in rest)
                     (match* (e1 e2)
                       ((`(forall ,_ (when (and ,@conditions1) ,atom1))
                         `(forall ,_ (when (and ,@conditions2) ,atom2)))
                        (when (and (covers parameters atoms atom1)
                                   (covers parameters atoms atom2)
                                   precond
                                   condition1
                                   condition2)
                          (return-from too-heavy-p t)))))))))))


(defun covers (parameters atoms atom)
  
                                   
                        
                     
          

(defun unbalanced-p (candidate parameters atoms)
  (ematch candidate
    ((candidate parameters atoms)
     (iter (for a in *actions*)
           (ematch a
             ((plist :parameters action-params :effect `(and ,@effects))
              (iter (for e in effects)
                    (match e
                      (`(forall ,_ (when ,_ (,(eq name) ,@_)))
                        
                        (return-from unbalanced-p t))
                      (`(forall ,_ (when ,_ (not (,(eq name) ,@_))))
                        (return-from unbalanced-p t))))))))))
