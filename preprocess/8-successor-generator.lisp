
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

(defmethod make-load-form ((sg sg-node) &optional env)
  (make-load-form-saving-slots sg :environment env))

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

(defparameter *sg-compilation-threashold* 3000
  "threashold for the number of operators, determining whether it should compile the successor generator")

(defmacro do-leaf ((op-id state sg) &body body &environment env)
  (assert (symbolp state))
  (assert (symbolp op-id))
  (assert (symbolp sg))
  (if (< *sg-compilation-threashold* (length *instantiated-ops*))
      (interpret-iteration-over-leaf op-id state (symbol-value sg) body)
      (compile-iteration-over-leaf op-id state (symbol-value sg) body)))

(defun compile-iteration-over-leaf (op-id-sym state-sym sg body
                                    &optional (start 0)
                                    &aux
                                      (end (min *state-size* (+ start 64)))
                                      (width (- end start)))
  "Returns a program that iterates over the leaf of sg, inlining constants, and execute BODY on each loop."
  (with-gensyms (pack)
    (labels ((rec (sg binding)
               (ematch sg
                 ((sg-node variable then else either)
                  (if (< variable end)
                      (if (and then else)
                          `((if (= 1 (aref ,state-sym ,variable))
                                ;; already checked this variable, so no longer to extend the binding
                                (progn ,@(rec then binding))
                                (progn ,@(rec else binding)))
                            ,@(rec either binding))
                          (append (rec then (cons (cons variable t) binding))
                                  (rec else (cons (cons variable nil) binding))
                                  (rec either binding)))
                      (wrap-check
                       binding
                       (lambda ()
                         (list (compile-iteration-over-leaf op-id-sym state-sym sg body end))))))
                 ((list* op-ids)
                  (wrap-check
                   binding
                   (lambda ()
                     (when op-ids
                       (if (< (length op-ids) 4)
                           (iter (for id in op-ids)
                                 (appending
                                  (subst id op-id-sym body))) 
                           (with-gensyms (i)
                             `((dotimes (,i ,(length op-ids))
                                 (let ((,op-id-sym (aref ,(make-array (length op-ids)
                                                                      :element-type 'op-id
                                                                      :initial-contents op-ids)
                                                         ,i)))
                                   ,@body)))))))))))
             (wrap-check (binding cont)
               (when-let ((branches (funcall cont)))
                 (if binding
                     (let ((mask 0) (compare 0) (negative-condition nil))
                       ;; pack 64bit masked comparison
                       (iter (for (var . val) in binding)
                             (for offset = (- var start))
                             (setf (ldb (byte 1 offset) mask) 1)
                             (if val
                                 (setf (ldb (byte 1 offset) compare) 1)
                                 (setf negative-condition t)))
                       (if negative-condition
                           ;; see note below
                           `((when (= 0 (logand ,mask (logxor ,compare ,pack)))
                               ,@branches))
                           `((when (= 0 (logand ,compare (lognot ,pack)))
                               ,@branches))))
                     branches))))
      `(let ((,pack (strips.lib::%packed-accessor-int ,state-sym ,width ,start)))
         ,@(rec sg nil)))))

;; note : logand+lognot saves additional storage for mask

;; (defun fn (x) (declare ((unsigned-byte 64) x)) (= 0 (LOGAND 35459518431232 (LOGNOT x))))
;; 
;; disassembly for FN
;; Size: 50 bytes. Origin: #x1001F40F64
;; ...
;; 6D:       488D141B         LEA RDX, [RBX+RBX]
;; 71:       4883F2FE         XOR RDX, -2
;; 75:       48231554FFFFFF   AND RDX, [RIP-172]               ; [#x1001F40ED0] = #x408020000000
;; 7C:       4885D2           TEST RDX, RDX
;; ...

;; (defun fn2 (x) (declare ((unsigned-byte 64) x)) (= 0 (logand 35459518431232 (LOGxor 35459518431232 x))))
;; 
;; disassembly for FN2
;; Size: 53 bytes. Origin: #x1001F40E78
;; 81:       488D141B         LEA RDX, [RBX+RBX]
;; 85:       48331554FFFFFF   XOR RDX, [RIP-172]               ; [#x1001F40DE0] = #x408020000000
;; 8C:       4823154DFFFFFF   AND RDX, [RIP-179]               ; [#x1001F40DE0] = #x408020000000
;; 93:       4885D2           TEST RDX, RDX
;; ...


(defun interpret-iteration-over-leaf (op-id-sym state-sym sg body)
  (log:warn "falling back to the interpretation based successor generation")
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
