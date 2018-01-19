
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; decoding state

(defun decode-state (state)
  (iter (for b in-vector state with-index i)
        (when (= 1 b)
          (collect (strips.lib:index-ref *fact-index* i)))))

(defun decode-op (op)
  (strips.lib:index-ref *op-index* op))
