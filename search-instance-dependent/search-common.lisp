
;; common search-related functions
;; TODO: applicable ops and apply-axioms are suboptimal.

(in-package :strips)

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))
  (ftype* initial-state+axioms *)
  (ftype* report-if-goal * * *)
  (ftype* applicable-ops * * *)
  (ftype* apply-axioms * *)
  (ftype* apply-axiom-layer * * *)
  (ftype* apply-op * * * *)
  (ftype* apply-effect * * * *))

(in-compilation-phase (phase/packed-structs)
(deftype op-id ()
  "maximum range *op-size* is an invalid op for the initial state"
  `(runtime integer 0 *op-size*))

(deftype fact-id ()
  "Including axioms. Maximum range *state-size* is an invalid fact"
  `(runtime integer 0 *state-size*))

(declaim (inline goalp))
(ftype* goalp state+axioms boolean)
(defun goalp (state+axioms)
  (= 1 (aref state+axioms (maybe-inline-obj *instantiated-goal*))))
)

(in-compilation-phase (phase/full-compilation)

(ftype* initial-state+axioms state+axioms)
(defun initial-state+axioms ()
  (let ((state (make-state+axioms)))
    (iter (for f in-vector *instantiated-init*)
          (setf (aref state f) 1))
    (apply-axioms state)
    state))

(ftype* report-if-goal state+axioms (function (&rest *) *) boolean)
(defun report-if-goal (state callback)
  (if (goalp state)
      (progn (restart-bind ((retrieve-path callback))
               (cerror "continue searching" 'goal-found))
             t)
      nil))
)

;; slow functions
(in-compilation-phase (phase/full-compilation)

(ftype* applicable-ops sg state+axioms (values (runtime simple-array 'op-id (list *op-size*)) op-id))
(defun applicable-ops (sg state)
  "Parse the successor generator. slow version"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((results (load-time-value
                  (make-array *op-size* :element-type 'op-id)))
        (c 0))
    (declare (op-id c)
             ((runtime simple-array 'op-id (list *op-size*)) results))
    (labels ((rec (node)
               (ematch node
                 ((sg-node variable then else either)
                  (if (= 1 (aref state variable))
                      (rec then)
                      (rec else))
                  (rec either))
                 ((type list)
                  (dolist (op-id node)
                    (setf (aref results c) op-id)
                    (incf c))))))
      (rec sg))
    (values results c)))

;; these functions are all destructive.

(ftype* apply-axioms state+axioms state+axioms)
(defun apply-axioms (state)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  #+(or)
  (map nil
       (lambda (layer)
         (apply-axiom-layer layer state))
       *instantiated-axiom-layers*)
  (iter (declare (iterate:declare-variables))
        (for layer in-vector *instantiated-axiom-layers*)
        (apply-axiom-layer layer state))
  state)

(ftype* apply-axiom-layer axiom-layer state+axioms state+axioms)
(defun apply-axiom-layer (axioms state) 
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((len (length axioms))
         (counters (load-time-value (make-array (length *ground-axioms*)
                                                :element-type 'fixnum))))
    ;; considered axioms get the counter value of -1
    ;; TODO: make it a load-time-value vector or make it dynamic-extent
    ;; (declare (dynamic-extent counters))
    (flet ((decrement (v counter)
             (if (if (minusp v)
                     (= 0 (aref state (lognot v)))
                     (= 1 (aref state v)))
                 (1- counter)
                 counter)))

      ;; initializing
      (dotimes (i len)
        (ematch (aref axioms i)
          ((effect con)
           (loop
              with counter fixnum = (length con)
              for v fixnum across con
              do
                (setf counter (decrement v counter))
              finally
              ;; I is below LEN
                (setf (aref counters i) counter)))))

      (let ((open nil))
        (loop ; initialize
           for i below len
           for c fixnum across counters
           do
             (when (zerop c) (push i open)))

        (do ((j (pop open) (pop open)))
            ((null j))
          (when (zerop (aref counters j)) ; re-evaluate, since it could be -1
            (decf (aref counters j))      ; -1
            (setf (aref state (effect-eff (aref axioms j))) 1)       ; achieve the axiom
            (loop
               for i below len
               for c fixnum across counters
               do
                 (when (plusp c)
                   (ematch (aref axioms i)
                     ((effect con)
                      (when (find j con) ; TODO: make it O(1)
                        (setf c (decrement j c))
                        (when (zerop c)
                          (push i open))
                        (setf (aref counters i) c)))))))))))
  state)

(ftype* apply-op op state+axioms state+axioms state+axioms)
(defun apply-op (op state child)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (ematch op
    ((op eff)
     #+(or)
     (map nil (lambda (e) (apply-effect e state child)) eff) ; somehow consing
     (iter (for e in-vector eff)
           (declare (iterate:declare-variables))
           (apply-effect e state child))
     child)))

(ftype* apply-effect effect state+axioms state+axioms state+axioms)
(defun apply-effect (effect state child)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (ematch effect
    ((effect con eff)
     (when (every (lambda (i) (or (and (minusp i)
                                       (= 0 (aref state (lognot i))))
                                  (= 1 (aref state i))))
                  con)
       (if (minusp eff)
           (setf (aref child (lognot eff)) 0)
           (setf (aref child eff) 1)))))
  child)
)


(in-compilation-phase (phase/full-compilation)
(ftype* applicable-ops/fast state+axioms (values (runtime simple-array 'op-id (list *op-size*)) op-id))
(defun applicable-ops/fast (state)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  #+(or)
  (in-compile-time (env)
    ;; checking macroexpansion (disabled)
    (print
     (macroexpand
      '(do-leaf (op-id state)
        (vector-push op-id results))))
    nil)
  (let ((results (load-time-value
                  (make-array *op-size* :element-type 'op-id)))
        (c 0))
    (declare (op-id c)
             ((runtime simple-array 'op-id (list *op-size*)) results))
    (do-leaf (op-id state *sg*)
      (setf (aref results c) op-id)
      (incf c))
    (values results c)))

(print-function-size 'applicable-ops/fast)
)

(in-compilation-phase (phase/full-compilation)
(ftype* apply-op/fast op-id state+axioms state+axioms state+axioms)
(defun apply-op/fast (op-id state child)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  #+(or)
  (in-compile-time (env)
    ;; checking macroexpansion (disabled)
    (print
     (macroexpand
      '(compiled-apply-op op-id state child)))
    nil)
  (compiled-apply-op op-id state child *instantiated-ops*))

(print-function-size 'apply-op/fast)
)


