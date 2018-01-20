
;; common search-related functions
;; TODO: applicable ops and apply-axioms are suboptimal.

(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(ftype* initialize-init state)
(defun initialize-init ()
  (let ((state (make-state)))
    (iter (for f in-vector *instantiated-init*)
          (setf (aref state f) 1))
    (apply-axioms state)
    state))

(define-condition goal-found (error)
  ((state :initarg :state)))

(ftype* report-if-goal state boolean)
(defun report-if-goal (state)
  (declare (state state))
  (if (= 1 (aref state *instantiated-goal*))
      (progn (cerror "continue searching" 'goal-found :state state)
             t)
      nil))

(ftype* applicable-ops sg state list)
(defun applicable-ops (sg state)
  "Parse the successor generator. slow version"
  (declare (state state))
  (declare (sg sg))
  (let ((results nil))
    (labels ((rec (node)
               (ematch node
                 ((type list) (appendf results node))
                 ((sg-node variable then else either)
                  (case (aref state variable)
                    (0 (rec else))
                    (1 (rec then)))
                  (rec either)))))
      (rec sg)
      (mapcar (lambda (op) (aref *instantiated-ops* op))
              results))))

;; these functions are all destructive.

(ftype* apply-axioms state state)
(defun apply-axioms (state)
  (map nil
       (lambda (layer)
         (apply-axiom-layer layer state))
       *instantiated-axiom-layers*)
  state)

(ftype* apply-axiom-layer axiom-layer state state)
(defun apply-axiom-layer (axioms state) 
  (let ((counters (make-array (length axioms) :element-type 'fixnum))
        (len (length axioms)))
    ;; considered axioms get the counter value of -1
    ;; TODO: make it a load-time-value vector or make it dynamic-extent
    ;; (declare (dynamic-extent counters))
    (flet ((decrement (v counter)
             (if (or (and (minusp v)
                          (= 0 (aref state (lognot v))))
                     (= 1 (aref state v)))
                 (1- counter)
                 counter)))
      
      (dotimes (i len)
        (ematch (aref axioms i)
          ((effect con)
           (loop
              with counter fixnum = (length con)
              for v fixnum across con
              do
                (setf counter (decrement v counter))
              finally
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
            (setf (aref state (+ *fact-size* j)) 1)       ; achieve the axiom
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

(ftype* apply-op op state state state)
(defun apply-op (op state child)
  (ematch op
    ((op eff)
     (map nil (lambda (e) (apply-effect e state child)) eff)
     child)))

(ftype* apply-effect effect state state state)
(defun apply-effect (effect state child)
  (ematch effect
    ((effect con eff)
     (when (every (lambda (i) (or (and (minusp i)
                                       (= 0 (aref state (lognot i))))
                                  (= 1 (aref state i))))
                  con)
       (if (minusp eff)
           (let ((i (lognot eff)))
             (setf (aref child i) 0))
           (setf (aref child eff) 1)))))
  child)

(deftype searcher ()
  '(function () t))

