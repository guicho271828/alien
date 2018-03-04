(in-package :strips)

#|

novelty heuristics

|#

(in-compilation-phase (phase/full-compilation)
(ftype* novelty1-heuristics state+axioms state+axioms (integer 0 1))
(defun novelty1-heuristics (state db)
  (let ((tmp (load-time-value (make-state+axioms))))
    ;; d  ~d s result
    ;; 0   1 0    0
    ;; 0   1 1    1 
    ;; 1   0 0    0
    ;; 1   0 1    0
    (bit-andc1 db state tmp)
    (prog1 (if (find 1 tmp) 0 (throw 'prune t))
      (bit-ior db state db))))

(declaim (inline make-novelty1-heuristics))
(defun make-novelty1-heuristics ()
  (let ((db (make-state+axioms)))
    (lambda (state)
      (novelty1-heuristics state db))))
)


(in-compilation-phase (phase/full-compilation)
(ftype* novelty2-heuristics
        state+axioms
        (runtime simple-array 'bit (list *state-size* *state-size*))
        (integer 1 3))
(defun novelty2-heuristics (state db)
  (let ((novelty 3))
    (declare ((integer 1 3) novelty))
    (iter (declare (declare-variables))
          (for i from 0)
          (while (< i (length state)))
          (when (= 1 (aref state i))
            (when (= 0 (aref db i i))
              ;; (format t "novelty 1 by i = ~a !~%" i)
              (minf novelty 1)
              (setf (aref db i i) 1))
            (iter (declare (declare-variables))
                  (for j from (1+ i))
                  (while (< j (length state)))
                  (when (= 1 (aref state j))
                    (when (= 0 (aref db i j))
                      ;; (format t "novelty 2 by i = ~a, j = ~a !~%" i j)
                      (minf novelty 2)
                      (setf (aref db i j) 1))))))
    ;; (print state)
    ;; (iter (for i below *state-size*)
    ;;       (iter (for j below *state-size*)
    ;;             (princ (aref db i j)))
    ;;       (terpri))
    (if (= novelty 3)
        (throw 'prune t)
        novelty)))

(declaim (inline make-novelty2-heuristics))
(defun make-novelty2-heuristics ()
  (let ((db (make-array (list *state-size* *state-size*)
                        :element-type 'bit
                        :initial-element 0)))
    (lambda (state)
      (novelty2-heuristics state db))))

)

#+(or)
(defun make-novelty-heuristics (k)
  (with-manager (:initial-num-vars-z (+ %mates %edges))
    ;; (set-zdd-variable-group :mtr-default :from 0 :size %mates)
    ;; (set-zdd-variable-group :mtr-default :from %mates :size %edges)
    ;; (print (dump-zdd-variable-group-hierarchy))
    ;; (zdd-enable-reordering :cudd-reorder-sift)
    (let ((f (zdd-set-of-emptyset)))
      (with-renaming ((+ zdd-union)
                      (* zdd-product-unate)
                      (/ zdd-divide-unate)
                      (! zdd-change)
                      (on  zdd-subset-1)
                      (off zdd-subset-0)
                      (% zdd-remainder-unate)
                      (_+ +)
                      (_* *)
                      (s zdd-singleton))
        (lambda (state)
          )))))
