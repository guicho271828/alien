
(in-package :alien)

#|

novelty heuristics local to a plateau

|#

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))
  (defun local-novelty1 (evaluator)
    (push 'local-novelty1 *optional-features*)
    (make-evaluator
     :storage '(list 'local-novelty1)
     :function `(lambda (state)
                  (funcall (load-time-value (make-local-novelty1-heuristics) t)
                           state
                           (funcall ,(evaluator-function evaluator) state)))))
  )

(in-compilation-phase ((and local-novelty1 phase/packed-structs))
  (alien.lib:define-packed-struct local-novelty1 ()
    (value 0 (integer 1 2))))

(in-compilation-phase ((and local-novelty1 phase/full-compilation))
  (defun make-local-novelty1-heuristics ()
    (let ((db (make-a-array 32
                            :element-type 'state+axioms
                            :initial-element (make-state+axioms))))
      (dotimes (i 32)
        (setf (aref db i) (make-state+axioms)))
      (lambda (state value)
        (local-novelty1-heuristics state db value))))
  
  (ftype* local-novelty1-heuristics state+axioms (array state+axioms) fixnum (integer 1 2))
  (defun local-novelty1-heuristics (state db value)
    (let ((tmp (make-state+axioms))
          (local-db (safe-aref db value (make-state+axioms))))
      (declare (dynamic-extent tmp))
      (bit-andc1 local-db state tmp)
      (prog1 (if (find 1 tmp) 1 2)
        (bit-ior local-db state local-db)))))

(in-compilation-phase ((not (or phase/packed-structs phase/full-compilation)))
(defun local-novelty2 (evaluator)
  (push 'local-novelty2 *optional-features*)
  (make-evaluator
   :storage '(list 'local-novelty2)
   :function `(lambda (state)
                (funcall (load-time-value (make-local-novelty2-heuristics) t)
                         state
                         (funcall ,(evaluator-function evaluator) state)))))
)

(in-compilation-phase ((and local-novelty2 phase/packed-structs))
  (alien.lib:define-packed-struct local-novelty2 ()
    (value 0 (integer 1 3))))

(in-compilation-phase ((and local-novelty2 phase/full-compilation))
  (defun make-local-novelty2-heuristics ()
    (let ((db (make-a-array 32
                            :element-type `(simple-array bit (,*state-size* ,*state-size*))
                            :initial-element (make-array (list *state-size* *state-size*)
                                                         :element-type 'bit
                                                         :initial-element 0))))
      (dotimes (i 32)
        (setf (aref db i) (make-array (list *state-size* *state-size*)
                                      :element-type 'bit
                                      :initial-element 0)))
      (lambda (state value)
        (local-novelty2-heuristics state db value))))

  (ftype* local-novelty2-heuristics
          state+axioms
          (array (runtime simple-array 'bit (list *state-size* *state-size*)))
          fixnum
          (integer 1 3))
  (defun local-novelty2-heuristics (state db value)
    (let ((novelty 3)
          (db (safe-aref db value (make-array (list *state-size* *state-size*)
                                              :element-type 'bit
                                              :initial-element 0))))
      (declare ((integer 1 3) novelty))
      (iter (declare (declare-variables))
            (for i from 0)
            (declare (fixnum i))
            (while (< i (length state)))
            (when (= 1 (aref state i))
              (when (= 0 (aref db i i))
                ;; (format t "novelty 1 by i = ~a !~%" i)
                (minf novelty 1)
                (setf (aref db i i) 1))
              (iter (declare (declare-variables))
                    (for j from (1+ i))
                    (declare (fixnum j))
                    (while (< j (length state)))
                    (when (= 1 (aref state j))
                      (when (= 0 (aref db i j))
                        ;; (format t "novelty 2 by i = ~a, j = ~a !~%" i j)
                        (minf novelty 2)
                        (setf (aref db i j) 1))))))
      novelty)))



