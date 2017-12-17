
;;; Invariant synthesys

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defmacro with-parsed-information (info &body body)
  "Binds the special variables using INFO, which is a parsed & flattened result of pddl files (see 2-translate.lisp).
   *types* *objects* *predicates* *init* *goal* *axioms* *actions* "
  `(match ,info
     ((plist :type *types*
             :objects *objects*
             :predicates *predicates*
             :init *init*
             :goal *goal*
             :axioms *axioms*
             :actions *actions*)
      ,@body)))

;; Usage:

#+(or)
(strips:with-parsed-information (strips:parse (strips:%rel "axiom-domains/opttel-adl-derived/p01.pddl"))
  strips:*types*)
#+(or)
(strips:with-parsed-information (strips:parse (strips:%rel "ipc2014-agl/transport-agl14/p01.pddl"))
  strips:*types*)
#+(or)
(strips:with-parsed-information (strips:parse (strips:%rel "ipc2011-opt/transport-opt11/p01.pddl"))
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

