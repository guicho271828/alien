(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(declaim (list *state-information-list*))
(defvar *state-information-list* nil)

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

(ftype* state-= state state boolean)
(defun state-= (s1 s2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; specialized to sb-int:bit-vector-=, then
  ;; inlined by defoptimizer, then
  ;; additionally, length comparison etc. are removed by the runtime type
  (equal s1 s2))

(ftype* state-hash state integer)
(defun state-hash (s)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sxhash s))

(sb-ext:define-hash-table-test state-= state-hash)

;; TODO static vectors, segmented vectors
(defun make-state (&optional temporary)
  "create a state **including** the axiom cells."
  (let ((s (make-array *state-size* :element-type 'bit)))
    (if temporary
        s
        (values s (register-state s)))))

;; (sb-vm:memory-usage :print-spaces t :count-spaces '(:aaa) :print-summary nil)

(declaim (strips.lib:index *close-list*))
(defvar *close-list*)

(defun make-close-list ()
  (strips.lib:make-index :test 'state-=))

(ftype* register-state state state-id)
(defun register-state (state)
  (or (strips.lib:index *close-list* state)
      (strips.lib:index-insert *close-list* state)))

(ftype* retrieve-state state-id state)
(defun retrieve-state (state-id)
  (strips.lib:index-ref *close-list* state-id))

;; TODO: idea: prune by bloom filter

