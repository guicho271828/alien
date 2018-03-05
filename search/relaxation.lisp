

(in-package :strips)

(deftype relaxer ()
  "a class of functions that returns a relaxed version of an operator."
  `(function (op) op))

(ftype* relaxed-sg (simple-array op) relaxer (values sg (simple-array op)))
(defun relaxed-sg (ops relaxer)
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
    (log:info "instantiating delete-relaxed successor generator")
    (setf (values *delete-relaxed-sg*
                  *delete-relaxed-ops*)
          (relaxed-sg *instantiated-ops* #'delete-relax-op)
          *delete-relaxed-op-size*
          (length *delete-relaxed-ops*))
    (log:info "~11@a: ~a" "op" (length *instantiated-ops*))
    (log:info "~11@a: ~a" "relaxed op" (length *delete-relaxed-ops*))))


(defvar *random-semi-relax-ratio* 0.8)

(ftype* random-semi-delete-relax-op op op)
(defun random-semi-delete-relax-op (op)
  (ematch op
    ((op pre eff)
     (let ((relaxed-pre (remove-if #'minusp pre))
           (relaxed-eff (random-semi-delete-relax-effects eff)))
       (make-op :pre relaxed-pre
                :eff relaxed-eff)))))

(ftype* random-semi-delete-relax-effects (array effect) (simple-array effect))
(defun random-semi-delete-relax-effects (effects)
  (coerce
   (iter (for e in-vector effects)
         (ematch e
           ((effect con eff)
            (if (minusp eff)
                (when (< *random-semi-relax-ratio* (random 1.0))
                  (collecting
                   (make-effect :con (remove-if #'minusp con)
                                :eff eff)
                   result-type vector))
                (collecting
                 (make-effect :con (remove-if #'minusp con)
                              :eff eff)
                 result-type vector)))))
   '(simple-array effect)))

(defun ensure-random-semi-delete-relaxed-sg ()
  (unless (symbol-value '*random-semi-delete-relaxed-sg*)
    (log:info "instantiating randomly semi-delete-relaxed successor generator")
    (setf (values *random-semi-delete-relaxed-sg*
                  *random-semi-delete-relaxed-ops*)
          (relaxed-sg *instantiated-ops* #'random-semi-delete-relax-op)
          *random-semi-delete-relaxed-op-size*
          (length *random-semi-delete-relaxed-ops*))
    (iter outer
          (for op in-vector *instantiated-ops*)
          (match op
            ((op eff)
             (iter (for e in-vector eff)
                   (match e
                     ((effect eff)
                      (in outer
                          (counting (minusp eff) into result)))))))
          (finally
           (log:info "~a deletes (original)" result)))
    (iter outer
          (for op in-vector *random-semi-delete-relaxed-ops*)
          (match op
            ((op eff)
             (iter (for e in-vector eff)
                   (match e
                     ((effect eff)
                      (in outer
                          (counting (minusp eff) into result)))))))
          (finally
           (log:info "~a deletes (semi-relaxed)" result)))
                 
             
    (log:info "~11@a: ~a" "op" (length *instantiated-ops*))
    (log:info "~11@a: ~a" "relaxed op" (length *random-semi-delete-relaxed-ops*))))
