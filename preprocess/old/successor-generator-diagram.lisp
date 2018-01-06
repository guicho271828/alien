
;; this idea was wrong because there might be different operators with same preconditions.


(defvar *sg-hash-test* 'equalp)
;; (defvar *sg-hash-test* 'sg-node-eq)

(defvar *sg-hash*)

(defun sg-node-eq (node1 node2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (sg-node node1 node2))
  (with-slots ((variable1 variable) (then1 then) (else1 else) (either1 either)) node1
    (with-slots ((variable2 variable) (then2 then) (else2 else) (either2 either)) node2
      (and (= variable1 variable2)
           (match* (then1 then2)
             (((fixnum) (fixnum)) (= then1 then2))
             (((sg-node) (sg-node)) (sg-node-eq then1 then2)))
           (match* (else1 else2)
             (((fixnum) (fixnum)) (= else1 else2))
             (((sg-node) (sg-node)) (sg-node-eq else1 else2)))
           (match* (either1 either2)
             (((fixnum) (fixnum)) (= either1 either2))
             (((sg-node) (sg-node)) (sg-node-eq either1 either2)))))))

(defun sg-node-hash (node)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (sg-node node))
  (with-slots (variable then else either) node
    (let ((result (sxhash variable)))
      (declare (type fixnum result))
      (sb-int:mixf result
                   (if (integerp then)
                       (locally (declare (fixnum then))
                         (sxhash then))
                       (sb-impl::eq-hash then)))
      (sb-int:mixf result
                   (if (integerp else)
                       (locally (declare (fixnum else))
                         (sxhash else))
                       (sb-impl::eq-hash else)))
      (sb-int:mixf result
                   (if (integerp either)
                       (locally (declare (fixnum either))
                         (sxhash either))
                       (sb-impl::eq-hash either)))
      result)))

(sb-ext:define-hash-table-test sg-node-eq sg-node-hash)
