(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(deftype state ()
  "vector representing a state, each bit is a proposition"
  `(simple-bit-vector ,(if (boundp '*state-size*)
                           *state-size*
                           '*)))

(defun make-state ()
  "create a state **including** the axiom cells."
  (make-array *state-size* :element-type 'bit))

(deftype close-list ()
  `(simple-bit-vector ,(if (boundp '*fact-size*)
                           (* *fact-size* maximum-states)
                           '*)))

(declaim (close-list *close-list*))
(defvar *close-list*)
(defun make-close-list (maximum-states)
  "create a very large bit vector, allocated initially.
This vector contains only the fact information, since
axiom information should be deduced from the axiom evaluator."
  (make-array (* *fact-size* maximum-states) :element-type 'bit))


(deftype state-id ()
  '(unsigned-byte 32))

(defun maximum-state-id ()
  (floor (* 8 1024 *memory-limit*)
         *fact-size*))


