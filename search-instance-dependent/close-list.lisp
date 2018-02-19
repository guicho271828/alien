
(in-package :strips)

#|

;;; close list

We implement an efficient lookup table STATE->ID 
without storing the copy of the state bit vector.

We only store the hash value and the ID of a state.

hashtable < hash, (list id) >

Since we have arrays of states (i.e. id -> state) we can check for the duplicates.

duplicate detection for a state:
  ids <- lookup (hashtable, hash)
  for each id: check equal(state, array[id])
  if none are equal, then new.


cf.

StateID StateRegistry::insert_id_or_pop_state()
using C++ unordered_set<StateID, StateIDSemanticHash, StateIDSemanticEqual>
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ compares the hash value of states

|#

#-(or strips::phase/packed-structs strips::phase/full-compilation)
(progn
  (ftype* state-= * * *)
  (ftype* state-hash * *)
  (ftype* make-state *)
  (ftype* make-state+axioms *)
  (ftype* close-list-insert * * *))

#+strips::phase/packed-structs
(progn
(deftype state ()
  "bitvector representing a state, each bit is a proposition"
  `(runtime simple-bit-vector *fact-size*))

(deftype state+axioms ()
  "bitvector representing a state including the axiom bits, each bit is a proposition"
  `(runtime simple-bit-vector *state-size*))

(defun max-state-id ()
  (unless (or *compile-file-pathname*
              *load-pathname*)
    (log:warn "Slow function: Avoid calling this function in runtime, especially in inner loop!"))
  (floor (* 8 1024 *memory-limit*) ; kB -> bit
         ;; (size-of 'state-information) ; -- circular dependency
         ;; using 64 bit as the lower bound
         64))

(define-compiler-macro max-state-id (&whole whole)
  (handler-case (max-state-id)
    (error () whole)))

(deftype state-id ()
  `(runtime integer 0 (max-state-id)))
)

#+strips::phase/full-compilation
(progn
(ftype* state-= state state boolean)
(defun state-= (s1 s2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; specialized to sb-int:bit-vector-=, then
  ;; inlined by defoptimizer, then
  ;; additionally, length comparison etc. are removed by the runtime type
  (equal s1 s2))

(ftype* state-hash state fixnum)
(defun state-hash (s)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (sxhash s))

(declaim (inline make-state make-state+axioms))
(ftype* make-state state)
(ftype* make-state+axioms state+axioms)
(defun make-state        () (error "slow runtime call to make-state!"))
(defun make-state+axioms () (error "slow runtime call to make-state+axioms!"))
(define-compiler-macro make-state        () `(make-array ,*fact-size* :element-type 'bit))
(define-compiler-macro make-state+axioms () `(make-array ,*state-size* :element-type 'bit))

(defun fixnum-= (a b)
  (declare (optimize (speed 3) (safety 0))
           (fixnum a b))
  (= a b))

(sb-ext:define-hash-table-test fixnum-= identity)

(defstruct (close-list (:constructor make-close-list (&rest hash-table-args
                                                            &key key-function
                                                            &allow-other-keys)))
  (key-function  (error "missing key-function")
                 :type (function (state-id) state))
  (counter       0
                 :type fixnum)
  (table (apply #'make-hash-table
                :allow-other-keys t
                ;; assumes the keys are already hash values
                :test 'fixnum-=
                hash-table-args)
         :type hash-table))

;; 3.4.1.4 Specifiers for keyword parameters
;; If more than one such argument pair matches, the leftmost argument pair is used.
;; 
;; (defun fn (&key a)
;;   (print a))
;; 
;; (fn :a 1 :a 2)
;; ; -> 1
;; thus :HASH-FUNCTION and :TEST is not overridden.

(ftype* close-list-insert close-list state (values state-id boolean))
(defun close-list-insert (close-list thing)
  "Inserts THING to the close-list under duplicate detection. Returns two values: an ID and a boolean.
If the secondary value is T, then the state is a duplicate."
  (ematch close-list
    ((close-list table
                 key-function
                 (counter (place counter)))
     
     (let* ((hash (state-hash thing))
            (bag (gethash hash table)))
       (if-let ((id (find thing bag
                          :key key-function
                          :test #'state-=)))
         ;; duplicate found, do not insert
         (values id t)
         ;; duplicate not found, return the current counter as an id and increment the counter
         (prog1 counter
           (setf (gethash hash table)
                 (cons counter bag))
           (incf counter)))))))
)
