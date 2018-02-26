(in-package :strips)

(defun goals ()
  "Returns a bit-vector whose bits are set when
the index corresponding to the fact/axiom is a condition for achieving the goal axiom.

This includes facts that derives an axiom that further enables another axioms and so on,
leading to achieveing the goal axiom.

The mark does not include the goal axiom itself.
"
  (let ((marked (make-array *state-size* :element-type 'bit)))
    (labels ((find-axiom (id)
               (iter (for axiom-layer in-vector *instantiated-axiom-layers*)
                     (iter (for e in-vector axiom-layer)
                           (when (= (effect-eff e) id)
                             (return-from find-axiom e)))))
             (rec (axiom)
               (ematch axiom
                 ((effect con)
                  (iter (for c in-vector con)
                        (let ((i (logabs c)))
                          (if (< i *fact-size*)
                              ;; is a fact
                              (setf (aref marked i) 1)
                              (when (= 0 (aref marked i))
                                (setf (aref marked i) 1)
                                (rec (find-axiom i))))))))))
      (rec (find-axiom *instantiated-goal*))
      marked)))

(defun non-axiom-goals ()
  (bit-and (goals)
           (let ((mask (make-array *state-size* :element-type 'bit)))
             (fill mask 1 :end *fact-size*))))


(defun compress-array/integer (list)
  "compress the given array to its smallest specialized element type"
  (if (emptyp list)
      (make-array 0 :element-type 'nil)
      (iter (for elem in list)
            (for i from 1)
            (minimizing elem into min)
            (maximizing elem into max)
            (finally
             (return
               (make-array i
                           :element-type `(integer ,min ,max)
                           :initial-contents list))))))

(defun goals-as-indices ()
  (compress-array/integer
   (iter (for goal? in-vector (goals) with-index i)
         (when (= 1 goal?)
           (collect i)))))

(defun non-axiom-goals-as-indices ()
  (compress-array/integer
   (iter (for goal? in-vector (non-axiom-goals) with-index i)
         (when (= 1 goal?)
           (collect i)))))


(defun achievers (fact-id ops)
  (assert (< fact-id *fact-size*))
  (compress-array/integer
   (iter (for op in-vector ops with-index op-id)
         (when (iter (for effect in-vector (op-eff op))
                     (thereis
                      (= fact-id (effect-eff effect))))
           (collecting op-id)))))
