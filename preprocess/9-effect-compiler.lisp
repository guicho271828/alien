(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun find-lexical-variables (env)
  (mapcar #'car
          (sb-c::lexenv-vars
           (sb-c::coerce-to-lexenv env))))

(defmacro fcase9 (i &body body &environment env)
  "Jump-table based CASE implementation by myself
See https://gist.github.com/guicho271828/707be5ad51edb858ff751d954e37c267 for summary"
  (let* ((vars (find-lexical-variables env))
         (types (mapcar (rcurry #'introspect-environment:variable-type env) vars)))
    `(funcall
      (the (function ,types *)
           (svref (the (simple-vector ,(length body))
                       (load-time-value
                        (vector
                         ,@(iter (for b in body)
                                 (for j from 0)
                                 (collecting
                                  `(lambda (,@vars) (locally (declare ((eql ,j) ,i)) ,b)))))
                        t))
                  ,i))
      ,@vars)))

;; Example
#+(or)
(defun 256way/fcase9 (i)
  (let ((rand (random 10)))
    (fcase9 i
      . #.(loop :for x :from 0 :repeat 256
             :collect `(progn (* i rand))))))

(defmacro compiled-apply-op (op-id state child ops)
  (assert (symbolp ops))
  (%compiled-apply-op op-id state child ops))

(defun %compiled-apply-op (op-id state child ops)
  `(interpret-fast-effect
    (aref ,(map 'vector #'compile-op (symbol-value ops)) ,op-id)
    ,state
    ,child))

(defun compile-op (op)
  (ematch op
    ((op eff)
     (let ((sg (effects-to-sg eff)))
       (let ((fe (sg-to-fast-effect sg)))
         fe)))))
         
(defun effects-to-sg (effects)
  "convert effects to an sg whose the leaf node contains fact ids instead of op-ids."
  (let ((current nil))
    (iter (for effect in-vector effects)
          (setf current (extend-sg/effect current effect)))
    current))

(defun extend-sg/effect (current effect)
  "similar to extend/sg, but used to compile effect"
  (ematch effect
    ((effect con eff)
     (labels ((rec (current con-index)
                (let* ((condition (safe-aref con con-index most-positive-fixnum))
                       (var (logabs condition)))
                  (cond
                    ((= condition most-positive-fixnum)
                     ;; no more conditions
                     (ematch current
                       ((type list) ; leaf branch
                        (if (member eff current)
                            current
                            (cons eff current)))
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

(deftype fast-effect-leaf () `(simple-array (unsigned-byte 64)))

(defstruct (fast-effect-node (:constructor fast-effect-node (variable then else either))
                             (:constructor make-fast-effect-node))
  "VARIABLE corresponds to an index of a fact.
THEN/ELSE/EITHER are child nodes correspinding to the condition for variable V being 1,0, or don't care.
A child node is a generator node or a sg node.
A generator node is just a list containing operator indices."
  (variable -1 :type fixnum)
  (then   (error "missing arg") :type (or fast-effect-node fast-effect-leaf))
  (else   (error "missing arg") :type (or fast-effect-node fast-effect-leaf))
  (either (error "missing arg") :type (or fast-effect-node fast-effect-leaf)))

(defmethod make-load-form ((fast-effect fast-effect-node) &optional env)
  (make-load-form-saving-slots fast-effect :environment env))

(deftype fast-effect () '(or fast-effect-node fast-effect-leaf))

(defun print-byte-sequence (sequence) 
  (map nil #'(lambda (x) (format t "~&~64,'0b~%" x)) sequence))

(defun sg-to-fast-effect (sg)
  (ematch sg
    ((type list)
     (iter (for c in (sort (copy-list sg) #'< :key #'logabs))
           (for var = (logabs c))
           (for offset = (mod var 64))
           (for start = (* 64 (floor var 64)))
           (for pstart previous start)
           (with results = nil)
           (with add-flag = nil)
           (with del-flag = nil)
           (with add = 0)
           (with del = (1- (expt 2 64)))
           (when (and pstart (< pstart start))
             (cond
               ((and add-flag del-flag)
                (push (logior (ash 3 32) pstart) results)
                (push add results)
                (push del results))
               (add-flag
                (push (logior (ash 1 32) pstart) results)
                (push add results))
               (del-flag
                (push (logior (ash 2 32) pstart) results)
                (push del results)))
             (setf add-flag nil
                   del-flag nil
                   add 0
                   del (1- (expt 2 64))))
           (if (minusp c)
               (setf del-flag t
                     (ldb (byte 1 offset) del) 0)
               (setf add-flag t
                     (ldb (byte 1 offset) add) 1))
           (finally
            (cond
              ((and add-flag del-flag)
               (push (logior (ash 3 32) start) results)
               (push add results)
               (push del results))
              (add-flag
               (push (logior (ash 1 32) start) results)
               (push add results))
              (del-flag
               (push (logior (ash 2 32) start) results)
               (push del results)))
            ;; (print-byte-sequence (reverse results))
            (return
              (make-array (length results)
                          :element-type '(unsigned-byte 64)
                          :initial-contents (reverse results))))))
    ((sg-node variable then else either)
     (fast-effect-node
      variable
      (sg-to-fast-effect then)
      (sg-to-fast-effect else)
      (sg-to-fast-effect either)))))

(declaim (notinline interpret-fast-effect))
(ftype* interpret-fast-effect fast-effect simple-bit-vector simple-bit-vector simple-bit-vector)
(defun interpret-fast-effect (fe state child)
  (labels ((rec (fe)
             (ematch fe
               ((type (simple-array (unsigned-byte 64)))
                (iter (declare (declare-variables))
                      (declare (fixnum c))
                      (with c = 0)
                      (while (array-in-bounds-p fe c))
                      (let* ((header (aref fe c))
                             (start (ldb (byte 32 0) header)))
                        (incf c)
                        (when (logbitp 32 header)
                          (setf (strips.lib::%packed-accessor-int-unsafe child 64 start)
                                (logior (aref fe c)
                                        (strips.lib::%packed-accessor-int-unsafe child 64 start)))
                          (incf c))
                        (when (logbitp 33 header)
                          (setf (strips.lib::%packed-accessor-int-unsafe child 64 start)
                                (logand (aref fe c)
                                        (strips.lib::%packed-accessor-int-unsafe child 64 start)))
                          (incf c)))))
               ((fast-effect-node variable then else either)
                (if (= 1 (aref state variable))
                    (rec then)
                    (rec else))
                (rec either)))))
    (rec fe)
    child))

