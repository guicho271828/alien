(in-package :strips)

#|

novelty heuristics

|#

(in-compilation-phase ((and novelty1 phase/full-compilation))
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


(in-compilation-phase ((and novelty2 phase/full-compilation))
(ftype* novelty2-heuristics
        state+axioms
        (runtime simple-array 'bit (list *state-size* *state-size*))
        (integer 1 3))
(defun novelty2-heuristics (state db)
  (let ((novelty 3))
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

(in-compilation-phase ((and novelty3 phase/full-compilation))
(ftype* novelty3-heuristics
        state+axioms
        (runtime simple-array 'bit (list *state-size* *state-size* *state-size*))
        (integer 1 4))
(defun novelty3-heuristics (state db)
  (let ((novelty 4))
    (declare ((integer 1 4) novelty))
    (iter (declare (declare-variables))
          (for i from 0)
          (declare (fixnum i))
          (while (< i (length state)))
          (when (= 1 (aref state i))
            (when (= 0 (aref db i i i))
              (minf novelty 1)
              (setf (aref db i i i) 1))
            (iter (declare (declare-variables))
                  (for j from (1+ i))
                  (declare (fixnum j))
                  (while (< j (length state)))
                  (when (= 1 (aref state j))
                    (when (= 0 (aref db i j j))
                      (minf novelty 2)
                      (setf (aref db i j j) 1))
                    (iter (declare (declare-variables))
                          (for k from (1+ j))
                          (declare (fixnum k))
                          (while (< k (length state)))
                          (when (= 1 (aref state k))
                            (when (= 0 (aref db i j k))
                              (minf novelty 3)
                              (setf (aref db i j k) 1))))))))
    (if (= novelty 4)
        (throw 'prune t)
        novelty)))

(declaim (inline make-novelty3-heuristics))
(defun make-novelty3-heuristics ()
  (let ((db (make-array (list *state-size* *state-size* *state-size*)
                        :element-type 'bit
                        :initial-element 0)))
    (lambda (state)
      (novelty3-heuristics state db))))

)

(in-compilation-phase ((and novelty4 phase/full-compilation))
(ftype* novelty4-heuristics
        state+axioms
        (runtime simple-array 'bit (list *state-size* *state-size* *state-size* *state-size*))
        (integer 1 5))
(defun novelty4-heuristics (state db)
  (let ((novelty 5))
    (declare ((integer 1 5) novelty))
    (iter (declare (declare-variables))
          (for i from 0)
          (declare (fixnum i))
          (while (< i (length state)))
          (when (= 1 (aref state i))
            (when (= 0 (aref db i i i i))
              (minf novelty 1)
              (setf (aref db i i i i) 1))
            (iter (declare (declare-variables))
                  (for j from (1+ i))
                  (declare (fixnum j))
                  (while (< j (length state)))
                  (when (= 1 (aref state j))
                    (when (= 0 (aref db i j j j))
                      (minf novelty 2)
                      (setf (aref db i j j j) 1))
                    (iter (declare (declare-variables))
                          (for k from (1+ j))
                          (declare (fixnum k))
                          (while (< k (length state)))
                          (when (= 1 (aref state k))
                            (when (= 0 (aref db i j k k))
                              (minf novelty 3)
                              (setf (aref db i j k k) 1))
                            (iter (declare (declare-variables))
                                  (for l from (1+ k))
                                  (declare (fixnum l))
                                  (while (< l (length state)))
                                  (when (= 1 (aref state l))
                                    (when (= 0 (aref db i j k l))
                                      (minf novelty 4)
                                      (setf (aref db i j k l) 1))))))))))
    (if (= novelty 5)
        (throw 'prune t)
        novelty)))

(declaim (inline make-novelty4-heuristics))
(defun make-novelty4-heuristics ()
  (let ((db (make-array (list *state-size* *state-size* *state-size* *state-size*)
                        :element-type 'bit
                        :initial-element 0)))
    (lambda (state)
      (novelty4-heuristics state db))))

)


;; for understanding the recursion...
#|

(defun fn (state novelty)
  (let ((len (length state)))
    (terpri)
    (labels ((rec (remaining-bits-to-collect
                   position)
               (format t "~&~vt~a" (1+ (- novelty remaining-bits-to-collect)) position)
               (if (< 0 remaining-bits-to-collect)
                   (let ((next (1- remaining-bits-to-collect)))
                     (iter (for i from (1+ position) below (- len next))
                           (when (= 1 (aref state i))
                             (rec next i))))
                   (format t " ~a" :terminal))))
      (rec novelty 0))))

(fn #*001101 1)
(fn #*001101 2)
(fn #*00110101 2)
(fn #*00110101 3)
(fn #*00110101 4)
|#

#+(or)
(in-compilation-phase ((and novelty phase/full-compilation))

(ftype* make-zdd-tuples state+axioms fixnum cl-cudd:node)
(defun make-zdd-tuples (state novelty)
  (labels ((rec (remaining-bits-to-collect
                 position)
             ;; (format t "~&~vt~a" (1+ (- novelty remaining-bits-to-collect)) position)
             (if (< 0 remaining-bits-to-collect)
                 (let ((next (1- remaining-bits-to-collect)))
                   (iter (for i from position below (- *state-size* next))
                         (with tmp = (zdd-emptyset))
                         (when (= 1 (aref state i))
                           (setf tmp (zdd-union tmp (zdd-change (rec next (1+ i)) i))))
                         (finally
                          (return tmp))))
                 (zdd-set-of-emptyset))))
    (rec novelty 0)))

(ftype* novelty-heuristics state+axioms cl-cudd:manager cl-cudd:node fixnum fixnum)
(defun novelty-heuristics (state *manager* db k)
  (let ((new-db db)
        (novelty k))
    (declare (cl-cudd:node new-db)
             (fixnum novelty))
    
    (iter (declare (declare-variables))
          (for n from 1 below k)
          ;; collect length=n tuples
          (for tuples = (make-zdd-tuples state n))
          (setf new-db (zdd-union tuples new-db))
          (when (= novelty k)
            (unless (node-equal (zdd-difference tuples db)
                                (zdd-emptyset))
              ;; tuples contain something new
              (setf novelty n))))

    (values novelty new-db)))

(declaim (inline make-novelty-heuristics))
(defun make-novelty-heuristics (&rest keys
                                &key
                                  (k *state-size*)
                                  (initial-num-slots 256)
                                  (cache-size 262144)
                                  (max-memory 0)
                                  (prune t))
  "If k is given and is a number, prune the nodes beyond that novelty."
  (declare (ignorable initial-num-slots cache-size max-memory))
  (remf keys :k)
  (remf keys :prune)
  (let* ((*manager* (apply #'manager-init
                           :initial-num-vars-z *state-size*
                           keys))
         (manager *manager*) ; capture lexically
         (db (zdd-set-of-emptyset))
         (initial-state t))
    (lambda (state)
      (multiple-value-bind (novelty new-db) (novelty-heuristics state manager db k)
        (setf db new-db)
        (if (and prune (not initial-state) (<= k novelty))
            (throw 'prune t)
            (progn
              (setf initial-state nil)
              novelty))))))
)

#+(or)
(progn
(ql:quickload :strips)
(sb-ext:gc :full t)
(in-package :strips)
(setf *state-size* 5)
(defparameter *fn* (make-novelty-heuristics :prune nil))

(assert (= (funcall *fn* #*00000) 5))
(assert (= (funcall *fn* #*00000) 5))
(assert (= (funcall *fn* #*00001) 1))
(assert (= (funcall *fn* #*00001) 5))
(assert (= (funcall *fn* #*00101) 1))
(assert (= (funcall *fn* #*00001) 5))
(assert (= (funcall *fn* #*00100) 5))
(assert (= (funcall *fn* #*01010) 1))
(assert (= (funcall *fn* #*01001) 2))
(assert (= (funcall *fn* #*01010) 5))
(assert (= (funcall *fn* #*01100) 2))
(assert (= (funcall *fn* #*00011) 2))
(assert (= (funcall *fn* #*00011) 5))
(assert (= (funcall *fn* #*00110) 2))
(assert (= (funcall *fn* #*00110) 5))
(assert (= (funcall *fn* #*00111) 3))
(assert (= (funcall *fn* #*00111) 5))
)
