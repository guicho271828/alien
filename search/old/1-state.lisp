(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

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
(defun make-state ()
  "create a state **including** the axiom cells."
  (make-array *state-size* :element-type 'bit))

;; (sb-vm:memory-usage :print-spaces t :count-spaces '(:aaa) :print-summary nil)

(deftype close-list ()
  'strips.lib:index)

(defun make-close-list ()
  (strips.lib:make-index :test 'state-=))

(ftype* register-state close-list state state-id)
(defun register-state (close-list state)
  (or (strips.lib:index-id close-list state)
      (strips.lib:index-insert close-list (copy-seq state))))

(ftype* retrieve-state close-list state-id state)
(defun retrieve-state (close-list state-id)
  (strips.lib:index-ref close-list state-id))

;; TODO: idea: prune by bloom filter



