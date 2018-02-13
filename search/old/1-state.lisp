(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)







;; TODO static vectors, segmented vectors


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



