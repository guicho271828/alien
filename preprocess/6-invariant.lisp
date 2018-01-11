
;; this file is inadequately named as invariant.lisp.
;; it is in fact only running the axiom-layer deduction.
;; the true invariant synthesis code is added in the future.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; (-> "ipc2011-opt/transport-opt11/p01.pddl"
;;   %rel
;;   parse
;;   easy-invariant
;;   ground
;;   print)
;; 
;; (-> "axiom-domains/opttel-adl-derived/p01.pddl"
;;   %rel
;;   parse
;;   easy-invariant
;;   ground
;;   print)

(defvar *facts*)
(defvar *ops*)
(defvar *ground-axioms*)
(defvar *axiom-layers*)

(defun mutex-invariant (info)
  (with-parsed-information3 info
    (list* :axiom-layers (axiom-layers)
           info)))

(defun axiom-layers (&optional (package (find-package :pddl)))
  (let* ((string (%axiom-layers))
         (list (let ((*package* package)) (read-from-string string)))
         (result (make-array 32 :element-type 'list :initial-element nil :adjustable t)))
    (iter (for (axiom layer) in list)
          (unless (array-in-bounds-p result layer)
            (adjust-array result (* 2 (length result))))
          (push axiom (aref result layer)))
    result))

(defun %axiom-layers ()
  (run-prolog
   `(,@(when (eq :swi *axiom-layer-prolog*)
         `((:- (use_module (library tabling))) ; swi specific
           (:- (style_check (- singleton)))))

       ;; bprolog missing max_list
     ,@(unless (eq :swi *axiom-layer-prolog*)
         `((max_list (list ?x) ?x)
           (:- (max_list (list* ?x ?rest) ?y)
               (max_list ?rest ?z)
               (is ?y (max ?x ?z)))))
         
     
     ,@(iter (for f in (append (union *init* *facts*) *ground-axioms*))
             (collecting
                 `(fact ,(ensure-zeroary-to-atom f))))
     ;; (:- (table (/ axiom-layer 2))) ; this is a suboptimal solution due to the possible SWI bug below
     ,@(axiom-layer-rules)
     ;; this causes an exception "No permission to append findall-bag `0' (continuation in findall/3 generator?)".
       ;; Not sure the reason --- SWI bug?
     ,@(unless (eq :swi *axiom-layer-prolog*)
         `((:- (table (/ axiom-layer-over-disjunctions 2)))))
     (:- (axiom-layer-over-disjunctions ?predicate ?i)
         (fact ?predicate)
         (findall ?j (axiom-layer ?predicate ?j) ?list)
         (max_list (list* 0 ?list) ?i))
     ;; 
     ,@(print-sexp)
     (:- (wrap ?goal)
         (write "(")
         (call ?goal)
         (write ")"))
     (:- main
         (wrap (forall (axiom-layer-over-disjunctions ?ga ?i)
                       (or (-> (atom ?ga)
                             ;; fix for zero-ary predicates
                             (print-sexp (list (list ?ga) ?i)))
                           (print-sexp (list ?ga ?i)))))
         halt))
   *axiom-layer-prolog* :args '("-g" "main") :input nil))

(defun axiom-layer-rules ()
  (mapcar (lambda-ematch
            (`(:derived ,predicate (and ,@body))
              `(:- (axiom-layer ,(ensure-zeroary-to-atom predicate) ?i)
                   (findall ?j
                            (and
                             ,@(iter (for c in body)
                                     (for ?layer = (gensym "?layer"))
                                     (collecting `(axiom-layer-over-disjunctions
                                                   ,(ensure-zeroary-to-atom
                                                     (if (positive c) c (second c))) ,?layer)
                                       into rule-body)
                                     (collecting ?layer
                                       into layer-vars)
                                     (finally
                                      (return
                                        (append rule-body
                                                ;; take the max over dependent axioms/predicates
                                                `((max_list (list ,@layer-vars) ?j)))))))
                            ?list)
                   ;; take the max over the combinations of assignments to existentially quiantified variables
                   (max_list (list* 0 ?list) ?i1)
                   (is ?i (+ ?i1 1)))))
          *axioms*))

