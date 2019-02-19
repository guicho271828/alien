(in-package :alien)
(named-readtables:in-readtable :fare-quasiquote)

;;; open list

;; slow, initial implementation of bucket open list as a placeholder for initial submission

(defstruct bucket-open-list
  "bucket open list with a single key, lifo tiebreaking; initial, slow implementation."
  (min-key most-positive-fixnum :type fixnum) ; initially out-of-bounds
  (buckets (make-array 32 :element-type 'list :initial-element nil :adjustable t)))

;; the state of min-key is either out-of-bounds or actual.

(defun bucket-open-list-insert (open key element)
  (ematch open
    ((bucket-open-list buckets :min-key (place min-key))
     (unless (array-in-bounds-p buckets key)
       (adjust-array buckets (expt 2 (integer-length key)) :initial-element nil))
     (push element (aref buckets key))
     (minf min-key key))))

(defun bucket-open-list-pop (open)
  (ematch open
    ((bucket-open-list buckets :min-key (place min-key))
     (when (= min-key most-positive-fixnum) 
       (error 'no-solution))
     (prog1 (pop (aref buckets min-key))
       (if-let ((pos (position-if #'identity buckets :start min-key)))
         (setf min-key pos)
         (setf min-key most-positive-fixnum))))))

(defun bucket-open-list (evaluator)
  (ematch evaluator
    ((evaluator storage function)
     (make-open-list
      :storage 'nil ; no cache
      :constructor '(function make-bucket-open-list)
      :insert `(lambda (open id state)
                 (bucket-open-list-insert
                  open
                  (funcall ,function state)
                  id))
      :pop '(function bucket-open-list-pop)))))

;; cached open list

(alien.lib:define-packed-struct cache-bit ()
  (cached 0 bit))

(defun cached-bucket-open-list (evaluator)
  (ematch evaluator
    ((evaluator storage function)
     (make-open-list
      :storage `(list* 'cache-bit ,storage)
      :constructor '(function make-bucket-open-list)
      :insert `(let ((info (make-state-information)))
                 (lambda (open id state)
                   (packed-aref *db* 'state-information id info)
                   (let ((value
                          (if (= 1 (state-information-cached info))
                              (state-information-value info)
                              (let ((new-key (funcall ,function state)))
                                (setf (state-information-value info) new-key
                                      (state-information-cached info) 1
                                      (packed-aref *db* 'state-information id) info)
                                new-key))))
                     (bucket-open-list-insert open value id))))
      :pop '(function bucket-open-list-pop)))))

;; lazy open list

(defun lazy-bucket-open-list (evaluator)
  (ematch evaluator
    ((evaluator storage function)
     (make-open-list
      :storage `(list* 'cache-bit ,storage)
      :constructor '(function make-bucket-open-list)
      :insert `(let ((info (make-state-information))
                     (pinfo (make-state-information)))
                 (lambda (open id state)
                   (packed-aref *db* 'state-information id info)
                   (let ((key
                          (if (= (state-information-op info) ,*op-size*)
                              ;; initial state, slot-size-of has a compiler macro
                              (ldb (byte (slot-size-of 'state-information 'value) 0) -1)
                              ;; other states
                              (let ((pid (state-information-parent info)))
                                (packed-aref *db* 'state-information pid pinfo)
                                (if (= 1 (state-information-cached pinfo))
                                    (state-information-value pinfo)
                                    (let ((pstate+axioms (make-state+axioms))
                                          (pstate        (make-state)))
                                      (declare (dynamic-extent pstate))
                                      (state-information-facts pinfo pstate)
                                      (replace pstate+axioms pstate)
                                      (apply-axioms pstate+axioms)
                                      (let ((new-key (funcall ,function pstate+axioms)))
                                        (setf (state-information-value pinfo) new-key
                                              (state-information-cached pinfo) 1
                                              (packed-aref *db* 'state-information pid) pinfo)
                                        new-key)))))))
                     (bucket-open-list-insert open key id))))
      :pop '(function bucket-open-list-pop)))))

