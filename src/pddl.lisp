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
  (let (*types*                         ;
        *objects*                       ;
        *predicates*                    ;
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
  (match domain
    ((assoc :types typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *types* parsed)
       (appendf *predicates*
                (mapcar (lambda-match
                          ((cons type _) `(,type o)))
                        parsed))
       (dolist (p parsed)
         (match p
           ((cons self (and parent (not 'object)))
            (push `((,parent ?o) (,self ?o)) *axioms*))))))))

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
  (match problem
    ((assoc :objects typed-def)
     (let ((parsed (parse-typed-def typed-def)))
       (appendf *objects* parsed)
       (dolist (pair parsed)
         (match pair
           ((cons o (and type (not 'object)))
            (push `(,type ,o) *init*))))))))

(defun grovel-predicates (domain)
  (match domain
    ((assoc :predicates predicates)
     (dolist (predicate predicates)
       (match predicate
         ((list* name typed-def)
          (let* ((parsed (parse-typed-def typed-def))
                 (w/o-type `(,name ,@(mapcar #'car parsed))))
            (push w/o-type *predicates*)
            (dolist (pair parsed)
              (match pair
                ((cons arg (and type (not 'object)))
                 (push `((,type ,arg) ,w/o-type) *axioms*)))))))))))

(defun grovel-init (problem)
  (match problem
    ((assoc :init predicates)
     (dolist (condition predicates)
       (match condition
         ((list* '= _)
          (format t "~&; skipping ~a" condition))
         (_
          (setf *predicates* (flatten-types condition))))))))

(defun grovel-goal (problem)
  (match problem
    ((assoc :goal condition)
     (setf *goal* (flatten-types condition)))))

(defun grovel-actions (domain)
  (dolist (action (find :action domain :key #'first))
    (match action
      ((list :action name :parameters params
             :precondition pre :effects eff)
       (let* ((parsed (parse-typed-def params))
              (w/o-type (mapcar #'car parsed))
              (type-conditions (mapcar (lambda-match ((cons arg type) `(,type ,arg))) parsed)))
         (push `(,name :parameters ,w/o-type
                       :precondition
                       (and ,@type-conditions
                            ,(flatten-types pre))
                       :effects ,eff)
               *actions*))))))

(defun grovel-axioms (domain)
  (dolist (derived (find :derived domain :key #'first))
    (match derived
      ((list :derived result conditon)
       (push (list result (flatten-types conditon)) *axioms*)))))

;;; parse3 --- forall/exists as axioms

;; should flatten the representation

