
(in-package :alien)
(named-readtables:in-readtable :fare-quasiquote)

;; decoding state

(defun decode-fact (index)
  (alien.lib:index-ref *fact-index* (logabs index)))

(defun decode-state (state)
  (iter (for b in-vector state with-index i)
        (when (= 1 b)
          (collect (decode-fact i)))))

(defun decode-op (op)
  (ematch op
    ((integer)
     (match (alien.lib:index-ref *op-index* op)
       ((list (list* name args) _)
        (list* (getf (find name *actions* :key #'second) :original-action)
               args))))
    ((op)
     (decode-op (position op *instantiated-ops*)))))

(defmacro enumerate (name &body name-or-value)
  "Define a type NAME which could take some values specified by name-or-value.
name-or-value is a list whose element is whether a SYMBOL for a constant variable, or a list (SYMBOL INTEGER).
In the first form, the value of enumeration starts from 0 and increments in the each element.
In the second form, the value is set to INTEGER and the later elements increments from this value.
You can mix both forms. "
  (iter (with counter = 0)
        (for elem in name-or-value)
        (ematch elem
          ((symbol)
           (collecting `(define-constant ,elem ,counter) into body)
           (collecting counter into values)
           (incf counter))
          ((list (and name (symbol)) (and int (integer)))
           (collecting `(define-constant ,name ,int) into body)
           (setf counter int)
           (collecting counter into values)
           (incf counter)))
        (finally
         (return
           `(progn
              (deftype ,name () '(member ,@values))
              ,@body)))))


(defun memory-usage ()
  (sb-ext:gc :full t)
  (sb-vm:memory-usage :print-spaces t :count-spaces '(:dynamic) :print-summary nil))

(defmacro with-memory-usage-diff ((&key (spaces '(:dynamic))) &body body)
  `(call-with-memory-usage-diff ',spaces (lambda () ,@body)))

(defun call-with-memory-usage-diff (spaces fn)
  (sb-ext:gc :full t)
  (let ((old (mapcar #'sb-vm::type-breakdown spaces)))
    (unwind-protect (funcall fn)
      (sb-ext:gc :full t)
      (let ((new (mapcar #'sb-vm::type-breakdown spaces)))
        (iter (for old-breakdown in old)
              (for new-breakdown in new)
              (for space in spaces)
              (format t "; Memory consumption in ~a:~%" space)
              (let ((diff (iter (for (byte count type) in new-breakdown)
                                (for (obyte ocount otype) = (find type old-breakdown :key #'third))
                                (unless otype
                                  (setf obyte 0 ocount 0))
                                (unless (zerop (- count ocount))
                                  (collecting (list (- byte obyte) (- count ocount) type))))))
                (iter (for (byte count type) in diff)
                      (with digits1 = (1+ (sb-ext:decimal-with-grouped-digits-width
                                           (reduce #'max (mapcar #'first diff)))))
                      (with digits2 = (1+ (sb-ext:decimal-with-grouped-digits-width
                                           (reduce #'max (mapcar #'second diff)))))
                      (format t "~&; ~v@:d bytes for ~v@:d ~a objects"
                                digits1 byte
                                digits2 count
                                type))))))))




(defun print-function-size (fn)
  "Print the binary size of a function. Used to check the size of compiled SG etc."
  (declare (symbol fn))
  (log:info "Segment size of ~a: ~a bytes"
            fn
            (reduce #'+ (sb-disassem:get-fun-segments (symbol-function fn)) :key #'sb-disassem:seg-length)))



(defmacro in-compilation-phase ((phase) &body body)
  "Utility macro: has the same effect as writing #+PHASE, but does not clutter the editor highlightation"
  (when (featurep phase)
    `(progn ,@body)))


(defmacro maybe-inline-obj (form)
  "If the form evaluates successfully in compile-time, then make the form into a load-time-value
(because laod-time-value should also succeed).
Otherwise leave the form as it is."
  (handler-case
    (let ((result (eval form)))
       (typecase result
         ((or number symbol string character) result)
         (t `(load-time-value ,form))))
    (error (c)
      (log:warn "failed to inine a form due to: ~a~%~a~% form: ~a" (type-of c) c form)
      form)))


(defmacro with-renaming (bindings &body body)
  (let ((tmps (make-gensym-list (length bindings))))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst tmp old body :test #'equalp)))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst new tmp body :test #'equalp))))
  `(progn ,@body))

(declaim (inline slide-if))
(defun slide-if (predicate array &key (start 0) (end (length array)))
  "poor man's delete-if"
  (declare (fixnum start end)
           (array array)
           (function predicate))
  (let ((j start))
    (declare (fixnum j))
    (dotimes (i end)
      (let ((elem (aref array i)))
        (when (funcall predicate elem)
          (setf (aref array j) elem)
          (incf j))))
    j))

;; visualizing zdd

;; (defun dump-zdd (path name f)
;;   (cl-cudd.baseapi:zdd-dump-dot
;;    (manager-pointer *manager*)
;;    (cl-cudd.baseapi:cudd-regular (node-pointer f))
;;    (namestring (make-pathname :name name :type "dot" :defaults path))))
;; 
;; (defun draw (f)
;;   (dump-zdd "." "dump" f)
;;   (uiop:run-program (format nil "dot dump.dot -Tpdf -o dump.pdf")))


(defun show-doc (specs)
  (format *error-output* "Synopsis:~%")
  (iter (for (key arg variable) in specs)
        (format *error-output*
                "~@{~40@a : ~a~%~}"
                (format nil "~(~{~a~^ | ~}~) ~a" (ensure-list key) arg)
                (or (documentation variable 'variable) "undocumented")
                ""
                (format nil "  default value: ~a"
                        (if (boundp variable)
                            (symbol-value variable)
                            "unspecified")))))

(defvar *search-option* '(ocl (bucket-open-list (ff/rpg)))
  "Search configuration.")

(defun main-search ()
  (with-memory-usage-diff ()
    (run
     (timeout
      (- *time-limit*
         ;; subtract the current runtime
         (/ (- (get-internal-real-time) *start-time*)
            internal-time-units-per-second))
      (eval *search-option*)))))

(defvar *aliases*
  '((blind
     (ocl (bucket-open-list (blind))))
    (goal-count
     (ocl (bucket-open-list (gc))))
    (ff
     (ocl (bucket-open-list (ff/rpg))))
    (novelty1
     (ocl (bucket-open-list (novelty1))))
    (novelty2
     (ocl (bucket-open-list (novelty2))))
    (novelty3
     (ocl (bucket-open-list (novelty3))))
    (novelty4
     (ocl (bucket-open-list (novelty4))))
    
    (bwfs2
     (ocl
      (bucket-open-list
       (tiebreak
        (novelty2)
        (ff/rpg)))))
    (bwfs3
     (ocl
      (bucket-open-list
       (tiebreak
        (novelty3)
        (ff/rpg)))))
    (bwfs4
     (ocl
      (bucket-open-list
       (tiebreak
        (novelty4)
        (ff/rpg)))))
    (wffw11
     (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty1)
          h
          (local-novelty1 h))))))
    (wffw22
     (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty2)
          h
          (local-novelty2 h))))))
    (wffw12
     (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty1)
          h
          (local-novelty2 h))))))
    (wffw21
     (ocl
      (bucket-open-list
       (evaluator-let ((h (ff/rpg)))
         (tiebreak
          (novelty2)
          h
          (local-novelty1 h))))))
    (alien
     (ocl (bucket-open-list (alien))))
    (alien+ff
     (ocl (bucket-open-list
           (tiebreak
            (alien)
            (ff/rpg)))))
    (novelty2+alien+ff
     (ocl (bucket-open-list
           (tiebreak
            (novelty2)
            (alien)
            (ff/rpg)))))
    (novelty3+alien+ff
     (ocl (bucket-open-list
           (tiebreak
            (novelty3)
            (alien)
            (ff/rpg)))))
    (alien/rpg
     (ocl (bucket-open-list (alien/rpg))))
    (alien/rpg+ff
     (ocl (bucket-open-list
           (tiebreak
            (alien/rpg)
            (ff/rpg)))))
    (novelty2+alien/rpg+ff
     (ocl (bucket-open-list
           (tiebreak
            (novelty2)
            (alien/rpg)
            (ff/rpg)))))
    (novelty3+alien/rpg+ff
     (ocl (bucket-open-list
           (tiebreak
            (novelty3)
            (alien/rpg)
            (ff/rpg)))))

    ;; ARIENAI!!! == impossible (japanese)
    (alieni
     (ocl (bucket-open-list
           (tiebreak
            (ff/rpg) ; opposite order
            (alien)))))
    (alieni/rpg
     (ocl (bucket-open-list
           (tiebreak
            (ff/rpg)
            (alien/rpg)))))
    )
  "Aliases for predefined search configurations.")

