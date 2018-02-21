
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; decoding state

(defun decode-state (state)
  (iter (for b in-vector state with-index i)
        (when (= 1 b)
          (collect (strips.lib:index-ref *fact-index* i)))))

(defun decode-op (op)
  (ematch op
    ((integer)
     (match (strips.lib:index-ref *op-index* op)
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
    (prog1 (funcall fn)
      (sb-ext:gc :full t)
      (let ((new (mapcar #'sb-vm::type-breakdown spaces)))
        (iter (for old-breakdown in old)
              (for new-breakdown in new)
              (for space in spaces)
              (format t "~&Memory consumption in ~a:~%" space)
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
                      (format t "~v@:d bytes for ~v@:d ~a objects~%"
                              digits1 byte
                              digits2 count
                              type))))))))




