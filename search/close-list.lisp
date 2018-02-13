
(in-package :strips.lib)
(named-readtables:in-readtable :fare-quasiquote)

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

(defstruct (close-list (:constructor make-close-list (&rest hash-table-args
                                                            &key hash-function test-function key-function
                                                            &allow-other-keys)))
  (hash-function #'sb-impl::equalp-hash
                 :type (function (*) fixnum))
  (test-function #'equalp
                 :type (function (* *) boolean))
  (key-function  #'identity
                 :type (function (*) *))
  (counter       0
                 :type fixnum)
  (table (apply #'make-hash-table
                :allow-other-keys t
                ;; assumes the keys are already hash values
                :hash-function #'identity
                :test (lambda (a b)
                        (declare (optimize (speed 3) (safety 0))
                                 (fixnum a b))
                        (= a b))
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

(defun close-list-insert (close-list thing)
  "Inserts THING to the close-list under duplicate detection. Returns an id"
  (ematch close-list
    ((close-list table
                 hash-function
                 test-function
                 key-function
                 (counter (place counter)))
     
     (let* ((hash (funcall hash-function thing))
            (bag (gethash hash table)))
       (if-let ((id (find thing bag
                          :key key-function
                          :test test-function)))
         ;; duplicate found, do not insert
         id
         ;; duplicate not found, return the current counter as an id and increment the counter
         (prog1 counter
           (setf (gethash hash table)
                 (cons counter bag))
           (incf counter)))))))

