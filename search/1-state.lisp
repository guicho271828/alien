(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *state-information-list*)

(defun storage-bit-per-state ()
  (+ *fact-size*
     (reduce #'+ *state-information-list*
             :key (lambda (array)
                    (-> array
                      array-element-type
                      sb-c::find-saetp
                      sb-vm:saetp-n-bits)))))

(defun maximum-states ()
  ;; tbp
  (floor (* 8 1024 *memory-limit*)
         (storage-bit-per-state)))

(deftype state ()
  "vector representing a state, each bit is a proposition"
  `(simple-bit-vector ,(runtime-type *state-size*)))

(deftype state-id ()
  '(unsigned-byte 32))

(deftype state-db ()
  `(simple-bit-vector ,(runtime-type (* *fact-size* (maximum-states)))))

(defstruct close-list
  (count 0 :type state-id)
  (array (make-bit-vector (* *fact-size* (maximum-states))) :type state-db))


(defun make-state ()
  "create a state **including** the axiom cells."
  (make-array *state-size* :element-type 'bit))

(declaim (close-list *close-list*))
(defvar *close-list*)

(ftype* register-state state state-id)
(defun register-state (state)
  (let* ((close *close-list*)
         (state-id (close-list-count close)))
    (declare (close-list close))
    (replace (close-list-array close) state
             :start1 state-id)
    (setf (close-list-count close) (1+ state-id))))

(ftype* retrieve-state state-id &optional state state)
(defun retrieve-state (state-id &optional (state (make-state) state-supplied-p))
  (unless state-supplied-p
    (simple-style-warning "consing a new state outside close-list"))
  (replace state (close-list-array *close-list*)
           :start2 state-id)
  state)

