(in-package :strips)

;; note: FF heuristic value is dependent on the operator ordering.

(in-compilation-phase ((and ff/rpg phase/packed-structs))
  (strips.lib:define-packed-struct ff ()
    (ff 0 (runtime integer 0 *delete-relaxed-op-size*))))

(in-compilation-phase ((and ff/rpg phase/full-compilation))
  ;; the range is inclusive. *delete-relaxed-op-size* means unreachable
  (ftype* ff-heuristic/rpg state+axioms (runtime integer 0 *delete-relaxed-op-size*)))
  
(in-compilation-phase ((and ff/rpg phase/full-compilation))
  (defun delete-relaxed-apply-op (op-id parent child)
    ;; only add some propositions
    (compiled-apply-op op-id parent child *delete-relaxed-ops*)))

(in-compilation-phase ((and ff/rpg phase/full-compilation))
  (defun ff-heuristic/rpg (state)
    "FF-heuristics based on a relaxed planning graph."
    (declare (optimize (speed 3) (safety 0) (space 3) (debug 0)))
    ;; using the same idea of layer-membership from JAIR-FF, p263
    (let* ((action-layer-membership (maybe-inline-obj
                                     (make-array *delete-relaxed-op-size*
                                                 :element-type '(runtime integer 0 *delete-relaxed-op-size*))))
           (fact-layer-membership (maybe-inline-obj
                                   (make-array *state-size*
                                               :element-type '(runtime integer 0 (1+ *delete-relaxed-op-size*)))))
           (current-layer 0))
      (fill action-layer-membership (maybe-inline-obj *delete-relaxed-op-size*)) ; infinity
      (fill fact-layer-membership   (maybe-inline-obj *delete-relaxed-op-size*)) ; infinity
      
      (iter (for fact in-vector state with-index i)
            (when (= fact 1) ; this includes axioms
              (setf (aref fact-layer-membership i) 0)))
      
      (let* ((state1 (make-state+axioms))
             (state2 (make-state+axioms)))
        (declare (dynamic-extent state1 state2))
        (replace state1 state)
        (replace state2 state)
        (loop
           (do-leaf (op-id state1 *delete-relaxed-sg*)
             (when (< current-layer (aref action-layer-membership op-id))
               ;; else, already applied
               (setf (aref action-layer-membership op-id) current-layer)
               (delete-relaxed-apply-op op-id state1 state2)))
           (apply-axioms state2)
           
           (when (equal state1 state2)
             (return-from ff-heuristic/rpg
               ;; i.e. infinity
               *delete-relaxed-op-size*))

           (incf current-layer)
           ;; destroy state1
           (bit-xor state1 state2 state1)
           (iter (declare (declare-variables))
                 (for changed-fact in-vector state1 with-index i)
                 (when (= 1 changed-fact) ; this includes axioms
                   (setf (aref fact-layer-membership i) current-layer)))
           (when (goalp state2)
             ;; continue to the relaxed plan extraction
             (return))
           ;; prepare for the next iteration
           (replace state1 state2)))
      
      ;; extract the cost from rpg, Figure 2, p264
      (let ((goals (maybe-inline-obj
                    (non-axiom-goals-as-indices)))
            (G (maybe-inline-obj
                (make-array (list *delete-relaxed-op-size*
                                  *fact-size*)
                            :element-type 'fact-id)))
            (end-counters (maybe-inline-obj
                           (make-array *delete-relaxed-op-size*
                                       :element-type 'fact-id)))
            ;; set of ops that achieves each goal
            (achievers (maybe-inline-obj
                        (map 'vector
                             (lambda (fact)
                               (achievers fact *delete-relaxed-ops*))
                             (iota *fact-size*))))
            (marker (maybe-inline-obj
                     (make-array (list *delete-relaxed-op-size*
                                       *fact-size*)
                                 :element-type 'bit)))
            (cost 0))

        (flet ((difficulty (op-id)
                 ;; do not consider the axioms.
                 (iter (declare (declare-variables))
                       (for pre in-vector (op-pre (aref (maybe-inline-obj *delete-relaxed-ops*) op-id)))
                       (when (< pre (maybe-inline-obj *fact-size*))
                         (summing (aref fact-layer-membership pre)))))
               (push-layer (layer fact)
                 (let ((c (aref end-counters layer)))
                   (setf (aref G layer c) fact
                         (aref end-counters layer) (1+ c)))))
          
          ;; initializing G
          (fill end-counters 0)
          (iter (declare (declare-variables))
                (for goal in-vector goals)
                (for layer = (aref fact-layer-membership goal))
                (push-layer layer goal))
          
          (fill (sb-kernel:%array-data marker) 0)
          (iter (declare (declare-variables))
                (for layer from current-layer downto 1) ; for i:= m...1
                (for layer-1 = (1- layer))
                (iter (declare (declare-variables))
                      (for i from 0)
                      (while (< i (aref end-counters layer))) ; this is checked each time
                      (for goal = (aref G layer i))
                      (when (= 1 (aref marker layer goal))
                        (next-iteration))
                      
                      (incf cost)
                      
                      (for achiever-id = 
                           (iter (declare (declare-variables))
                                 (for op-id in-vector (aref achievers goal))
                                 (when (= layer-1 (aref action-layer-membership op-id))
                                   (finding op-id
                                            minimizing (difficulty op-id)))))
                      (for achiever = (aref *delete-relaxed-ops* achiever-id))
                      (iter (declare (declare-variables))
                            (for pre in-vector (op-pre achiever))
                            (for l = (aref fact-layer-membership pre))
                            (when (/= 0 l)
                              (push-layer l pre)))

                      (iter (declare (declare-variables))
                            (for eff in-vector (op-eff achiever))
                            (for add = (effect-eff eff))
                            (setf (aref marker layer   add) 1
                                  (aref marker layer-1 add) 1)))))
        cost))))


