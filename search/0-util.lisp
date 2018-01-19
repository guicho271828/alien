
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; decoding state

(defun decode-state (state)
  (iter (for b in-vector state with-index i)
        (when (= 1 b)
          (collect (strips.lib:index-ref *fact-index* i)))))

(defun decode-op (op)
  (strips.lib:index-ref *op-index* op))

(defun memory-usage ()
  (sb-ext:gc :full t)
  (sb-vm:memory-usage :print-spaces t :count-spaces '(:dynamic) :print-summary nil))

;; (defstruct a b c)

;; 384
;; 352
;; _32
;; 
;; 1 fixnum: 8 bytes
;; i.e. 4 slots per struct

;; (defstruct (v (:type vector)) b c)
;; (defstruct (v2 (:type vector))
;;   (b 0 :type fixnum)
;;   (c 0 :type fixnum))

;; simple-vector also consumes 32 bytes
;; additional 2 words for each structure

;; (defstruct (v3 (:type (array fixnum)))
;;   (b 0 :type fixnum)
;;   (c 0 :type fixnum))

;; (defun v2-b* (v2)
;;   (declare (vector v2)
;;            (optimize (safety 0) (speed 3) (debug 0)))
;;   (v2-b v2))
;; 
;; (defun v2-c* (v2)
;;   (declare (vector v2)
;;            (optimize (safety 0) (speed 3) (debug 0)))
;;   (v2-c v2))
;; 
;; (defun v-b* (v)
;;   (declare (vector v)
;;            (optimize (safety 0) (speed 3) (debug 0)))
;;   (v-b v))
;; 
;; (defun v-c* (v)
;;   (declare (vector v)
;;            (optimize (safety 0) (speed 3) (debug 0)))
;;   (v-c v))
