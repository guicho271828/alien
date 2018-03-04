(in-package :strips)

#|

novelty heuristics

|#

(ftype* make-novelty1-heuristics (function (state+axioms) (integer 0 1)))
(defun make-novelty1-heuristics ()
  (let ((db (make-state+axioms))
        (tmp (make-state+axioms)))
    (lambda (state)
      ;; d  ~d s result
      ;; 0   1 0    0
      ;; 0   1 1    1 
      ;; 1   0 0    0
      ;; 1   0 1    0
      (bit-andc1 db state tmp)
      (prog1 (if (find 1 tmp) 0 1)
        (bit-ior db state db)))))

(ftype* make-novelty2-heuristics (function (state+axioms) (integer 1 3)))
(defun make-novelty2-heuristics ()
  (let ((db (make-array (list *state-size* *state-size*)
                        :element-type 'bit
                        :initial-element 0)))
    (declare ((runtime simple-array 'bit (list *state-size* *state-size*)) db))
    (lambda (state)
      (let ((novelty 3))
        (iter (for b1 in-vector state with-index i)
              (when (aref state i)
                (when (= 0 (aref db i i))
                  (minf novelty 1)
                  (setf (aref db i i) 1))
                (iter (for b2 in-vector state with-index j from (1+ i))
                      (when (aref state j)
                        (when (= 0 (aref db i j))
                          (minf novelty 2)
                          (setf (aref db i j) 1))))))
        novelty))))
