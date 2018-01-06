
;; Successor generator is originally a structure similar to decision-tree.
;; Its internal nodes are selector nodes which have several children where each child
;; corresponds to a value of a SAS variable.
;; The leaf nodes are generator nodes.

;; Ours does not handle SAS encoding, and rather a binary encoding.
;; each node has then/else/either branches. to generate a list of applicable operators,
;; traverse the tree as follows:
;; When the current value of the variable is true, follow THEN branch and EITHER branch.
;; When the current value of the variable is false, follow ELSE branch and EITHER branch.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *sg*)

(defstruct (sg-node (:constructor sg-node (variable then else either)))
  "VARIABLE corresponds to an index of a fact.
THEN/ELSE/EITHER are child nodes correspinding to the condition for variable V being 1,0, or don't care.
A child node is either a number corresponding to an operator index, or a sg node. "
  (variable -1 :type fixnum)
  (then -1 :type (or sg-node fixnum))
  (else -1 :type (or sg-node fixnum))
  (either -1 :type (or sg-node fixnum)))

(define-constant +zero-node+ -1)

(defun generate-sg (instantiated-ops)
  (let ((current +zero-node+))
    (iter (for op in-vector instantiated-ops with-index i)
          (setf current (extend-sg current op i)))
    current))

(defun extend-sg (current op index)
  (match op
    ((op pre)
     (labels ((rec (current con-index)
                (let* ((condition (aref pre con-index))
                       (var (if (minusp condition)
                                (lognot condition)
                                condition))))
                  (cond
                    ((= condition most-positive-fixnum)
                     ;; no more conditions
                     index)
                    
                    ((eql current +zero-node+)
                     (cond
                       ((minusp condition)
                        ;; negative condition
                        (sg-node var +zero-node+ (rec +zero-node+ (1+ con-index)) +zero-node+))
                       (t
                        ;; positive condition
                        (sg-node var (rec +zero-node+ (1+ con-index)) +zero-node+ +zero-node+))))
                    
                    (t
                     (ematch current
                       ((sg-node variable then else either)
                        (cond
                          ((= var variable)
                           (if (minusp condition)
                               (sg-node var then (rec else (1+ con-index)) either)
                               (sg-node var (rec then (1+ con-index)) else either)))
                          ((< var variable)
                           (if (minusp condition)
                               (sg-node var current (rec +zero-node+ (1+ con-index)) current)
                               (sg-node var (rec +zero-node+ (1+ con-index)) current current))))))))))
       (rec current 0)))))

(defun applicable-ops (sg state)
  (declare (simple-bit-vector state))
  (let ((results nil))
    (labels ((rec (node)
               (ematch node
                 ((integer) (push node results))
                 ((sg-node variable then else either)
                  (case (aref state variable)
                    (0 (rec else))
                    (1 (rec then)))
                  (rec either)))))
      (rec sg)
      results)))

