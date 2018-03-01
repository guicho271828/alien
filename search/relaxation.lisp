

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
  (let ((relaxed-ops (map 'vector relaxer ops)))
    (setf relaxed-ops (delete-duplicates relaxed-ops :test #'equalp))
    (values (generate-sg relaxed-ops)
            (coerce relaxed-ops
                    '(simple-array op)))))

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

(defun ensure-delete-relaxed-sg ()
  (unless (symbol-value '*delete-relaxed-sg*)
    (setf (values *delete-relaxed-sg*
                  *delete-relaxed-ops*)
          (relax-sg *sg* *instantiated-ops* #'delete-relax-op)
          *delete-relaxed-op-size*
          (length *delete-relaxed-ops*))
    (log:info "~11@a: ~a" "op" (length *instantiated-ops*))
    (log:info "~11@a: ~a" "relaxed op" (length *delete-relaxed-ops*))))
