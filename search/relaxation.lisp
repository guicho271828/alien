

(in-package :strips)

(deftype relaxer ()
  "a class of functions that returns a relaxed version of an operator."
  `(function (op) op))

(ftype* relax-sg sg (simple-array op) relaxer (values sg (simple-array op)))
(defun relax-sg (sg ops relaxer)
  "Relaxes a SG using a relaxer function.
Returns two values: a relaxed SG and a vector of relaxed ops.
The original SG and operators are not destructively modified.
Operators with no effects are removed from the results and does not belong to the SG."
  (let ((relaxed-ops (make-a-array *op-size*)))
    (labels ((rec (sg)
               (ematch sg
                 ((sg-node variable then else either)
                  (sg-node variable
                           (rec then)
                           (rec else)
                           (rec either)))
                 ((list* op-ids)
                  (iter (for id in op-ids)
                        (let ((relaxed-op (funcall relaxer (aref ops id))))
                          (unless (emptyp (op-eff relaxed-op))
                            (collecting
                             (fill-pointer relaxed-ops))
                            (linear-extend relaxed-ops relaxed-op))))))))
      (values (rec sg)
              (copy-array relaxed-ops
                          :element-type 'op
                          :fill-pointer nil
                          :adjustable nil)))))

(ftype* delete-relax-op op op)
(defun delete-relax-op (op)
  (ematch op
    ((op pre eff)
     (let ((relaxed-pre (remove-if #'minusp pre))
           (relaxed-eff (delete-relax-effects eff)))
       (make-op :pre relaxed-pre
                :eff relaxed-eff)))))

(ftype* delete-relax-effects (array effect) (simple-array effect))
(defun delete-relax-effects (effects)
  (coerce
   (iter (for e in-vector effects)
         (ematch e
           ((effect con eff)
            (unless (minusp eff)
              (collecting
               (make-effect :con (remove-if #'minusp con)
                            :eff eff)
               result-type vector)))))
   '(simple-array effect)))

(defvar *delete-relaxed-sg* nil "Relaxed successor generators.")
(defvar *delete-relaxed-ops* nil "Relaxed operators.")
(defvar *delete-relaxed-op-size* nil "Relaxed operator size.")

(defun ensure-delete-relaxed-sg ()
  (unless (symbol-value '*delete-relaxed-sg*)
    (setf (values *delete-relaxed-sg*
                  *delete-relaxed-ops*)
          (relax-sg *sg* *instantiated-ops* #'delete-relax-op)
          *delete-relaxed-op-size*
          (length *delete-relaxed-ops*))))
