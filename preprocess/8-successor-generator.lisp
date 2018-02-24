
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

(defstruct (sg-node (:constructor sg-node (variable then else either))
                    (:constructor make-sg-node))
  "VARIABLE corresponds to an index of a fact.
THEN/ELSE/EITHER are child nodes correspinding to the condition for variable V being 1,0, or don't care.
A child node is a generator node or a sg node.
A generator node is just a list containing operator indices."
  (variable -1 :type fixnum)
  (then nil :type (or sg-node list))
  (else nil :type (or sg-node list))
  (either nil :type (or sg-node list)))

(deftype sg () '(or list sg-node))

(defun generate-sg (instantiated-ops)
  (let ((current nil))
    (iter (for op in-vector instantiated-ops with-index i)
          (setf current (extend-sg current op i)))
    current))

(defun extend-sg (current op index)
  ;; current: current sg node, initially the root node
  ;; op: operator to add to the sg
  (match op
    ((op pre)
     (labels ((rec (current con-index)
                (let* ((condition (safe-aref pre con-index most-positive-fixnum))
                       (var (logabs condition)))
                  (cond
                    ((= condition most-positive-fixnum)
                     ;; no more conditions
                     (ematch current
                       ((type list) ; leaf branch
                        (if (member index current)
                            current
                            (cons index current)))
                       ((sg-node variable then else either) ; inner node
                        (sg-node variable then else (rec either (1+ con-index))))))
                    
                    ((null current)
                     (cond
                       ((minusp condition)
                        ;; negative condition
                        (sg-node var nil (rec nil (1+ con-index)) nil))
                       (t
                        ;; positive condition
                        (sg-node var (rec nil (1+ con-index)) nil nil))))
                    (t
                     (ematch current
                       ((type list)
                        (if (minusp condition)
                            (sg-node var nil (rec nil (1+ con-index)) current)
                            (sg-node var (rec nil (1+ con-index)) nil current)))
                       ((sg-node variable then else either)
                        (cond
                          ((= var variable)
                           (if (minusp condition)
                               (sg-node var then (rec else (1+ con-index)) either)
                               (sg-node var (rec then (1+ con-index)) else either)))
                          ((< var variable)
                           (if (minusp condition)
                               (sg-node var nil (rec nil (1+ con-index)) current)
                               (sg-node var (rec nil (1+ con-index)) nil current)))
                          ((< variable var)
                           (sg-node variable then else (rec either con-index)))))))))))
       (rec current 0)))))

(defvar *sg-compilation-threashold* 10000
  "threashold for the number of operators, determining whether it should compile the successor generator")

(defmacro do-leaf ((op-id state) &body body &environment env)
  (once-only (state)
    (compile-iteration-over-leaf op-id state *sg* body)))

(defun compile-iteration-over-leaf (op-id-sym state-sym sg body)
  "Returns a program that iterates over the leaf of sg, inlining constants, and execute BODY on each loop."
  (ematch sg
    ((sg-node variable then else either)
     #+(or)
     `(progn
        ;; this is OK but the expansion becomes longer and difficult to debug
        (if (= 1 (aref ,state-sym ,variable))
            ,(compile-iteration-over-leaf op-id-sym state-sym then body)
            ,(compile-iteration-over-leaf op-id-sym state-sym else body))
        ,(compile-iteration-over-leaf op-id-sym state-sym either body))
     
     (flet ((ok (form)
              (not (equal form '(progn)))))
       (let* ((then-form (compile-iteration-over-leaf op-id-sym state-sym then body))
              (else-form (compile-iteration-over-leaf op-id-sym state-sym else body))
              (either-form (compile-iteration-over-leaf op-id-sym state-sym either body))
              (conditional-form
               (match* ((ok then-form) (ok else-form))
                 ((t t)
                  `(if (= 1 (aref ,state-sym ,variable))
                       ,then-form
                       ,else-form))
                 ((t nil) `(if (= 1 (aref ,state-sym ,variable)) ,then-form))
                 ((nil t) `(if (= 0 (aref ,state-sym ,variable)) ,else-form))
                 ((nil nil)
                  ;; equal to (progn)
                  then-form))))
         (match* ((ok conditional-form) (ok either-form))
           ((t t)
            `(progn ,conditional-form ,either-form))
           ((t nil) conditional-form)
           ((nil t) either-form)
           ((nil nil)
            ;; equal to (progn)
            then-form)))))
    ((list id)
     ;; special case for single element
     (compile-leaf id op-id-sym body))
    ((list* op-ids)
     `(progn
        ,@(iter (for id in op-ids)
                (collecting
                 (compile-leaf id op-id-sym body)))))))

(defun compile-leaf (id op-id-sym body)
  ;; I don't do this usually, but only for now
  ;; `(let ((,op-id-sym ,id)) ,@body)
  (let ((result (subst id op-id-sym body)))
    (if (second result)
        `(progn ,@result)
        (first result))))

(defun interpret-iteration-over-leaf (op-id-sym state-sym sg body)
  `(labels ((rec (node)
              (ematch node
                ((type list)
                 (dolist (,op-id-sym node)
                   ,@body))
                ((sg-node variable then else either)
                 (if (= 1 (aref ,state-sym variable))
                     (rec then)
                     (rec else))
                 (rec either)))))
     (rec ,sg)))
