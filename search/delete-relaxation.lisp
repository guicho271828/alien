
(in-package :strips)

#|

Construct a delete-relaxed successor generator and
delete-relaxed operators.

|#

(ftype* relax-sg sg (values sg (simple-array op)))
(defun relax-sg (sg)
  (let ((relaxed-ops (make-a-array (length *instantiated-ops*))))
    (labels ((rec (sg)
               (ematch sg
                 ((sg-node variable then else either)
                  (sg-node variable
                           (rec then)
                           (rec else)
                           (rec either)))
                 ((list* op-ids)
                  (iter (for id in op-ids)
                        (when-let ((relaxed-op (relax-op (aref *instantiated-ops* id))))
                          (collecting
                           (fill-pointer relaxed-ops))
                          (linear-extend relaxed-ops relaxed-op)))))))
      (values (rec sg)
              (coerce relaxed-ops '(simple-array op))))))

(ftype* relax-op op (or op null))
(defun relax-op (op)
  (ematch op
    ((op pre eff)
     (let ((relaxed-pre (remove-if #'minusp pre))
           (relaxed-eff (relax-effects eff)))
       (unless (emptyp relaxed-eff)
         (make-op :pre relaxed-pre
                  :eff relaxed-eff))))))

(ftype* relax-effects (array effect) (simple-array effect))
(defun relax-effects (effects)
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
