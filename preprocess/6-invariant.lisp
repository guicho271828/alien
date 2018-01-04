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

(defun axiom-layers (&optional debug)
  (let* ((string (%axiom-layers debug))
         (list (read-from-string string))
         (result (make-array 32 :element-type 'list :initial-element nil :adjustable t)))
    (iter (for (axiom layer) in list)
          (unless (array-in-bounds-p result layer)
            (adjust-array result (* 2 (length result))))
          (push axiom (aref result layer)))
    result))

(defun %axiom-layers (&optional debug)
  (run-prolog
   `((:- (use_module (library tabling))) ; swi specific
     (:- (style_check (- singleton)))
     ,@(iter (for f in (append (union *init* *facts*) *ground-axioms*))
             (collecting
                 `(fact ,f)))
     ,@(axiom-layer-rules)
     (:- (table (/ axiom-layer-over-disjunctions 2)))
     (:- (axiom-layer-over-disjunctions ?i ?predicate)
         (fact ?predicate)
         (findall ?j (axiom-layer ?j ?predicate) ?list)
         (max_list (list* 0 ?list) ?i))
     ;; 
     ,@(print-sexp :swi t)
     (:- (wrap ?goal)
         (write "(")
         (call ?goal)
         (write ")"))
     (:- main
         (wrap (forall (axiom-layer-over-disjunctions ?i ?ga)
                       (and (print-sexp (list ?ga ?i)) nl)))
         halt))
   :swi :args '("-g" "main") :debug debug))

(defun axiom-layer-rules ()
  (mapcar (lambda-ematch
            (`(:derived ,predicate (and ,@body))
              `(:- (axiom-layer ?i ,predicate)
                   (findall ?j
                            (and
                             ,@(iter (for c in body)
                                     (for ?layer = (gensym "?layer"))
                                     (collecting `(axiom-layer-over-disjunctions ,?layer ,(if (positive c) c (second c)))
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



