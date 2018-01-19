
(in-package :strips.lib)

;; X -> classid
;; classid -> [X]

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (equivalence (:constructor %make-equivalence))
    (total    -1 :type fixnum)
    (obsolete -1 :type fixnum)
    (groups (make-array 32 :element-type 'list :initial-element nil :adjustable t :fill-pointer 0)
            :type (array list))
    (hash (make-hash-table) :type hash-table)))

(defun delete-class (ec class)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence :obsolete (place obsolete) :groups groups)
     (setf (aref groups class) nil)
     (incf obsolete)
     ;; maybe shrink the vector later
     )))

(defun class-id (ec x)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence hash :total (place total) :groups groups)
     (or (gethash x hash)
         (let ((class (incf total)))
           (setf (gethash x hash) class)
           (vector-push-extend (list x) groups (array-total-size groups))
           class)))))

(defun add-relation (ec x y)
  (declare (equivalence ec))
  (ematch ec
    ((equivalence hash :obsolete (place obsolete) :groups groups)
     (let ((xc (class-id ec x))
           (yc (class-id ec y)))
       (when (> xc yc)
         (rotatef x y)
         (rotatef xc yc))
       (dolist (y (aref groups yc))
         (setf (gethash y hash) xc))
       (nconcf (aref groups xc)
               (aref groups yc))
       (delete-class ec yc)))))

(defun compute-mapping (ec)
  "Returns a plist from each variable to a constant"
  (declare (equivalence ec))
  (ematch ec
    ((equivalence hash :obsolete (place obsolete) :groups groups)
     (let ((mapping nil))
       ;; find the representative
       (iter (for elems in-vector groups with-index class)
             (for constant = nil)
             (for variables = nil)
             (dolist (e elems)
               (if (variablep e)
                   (push e variables)
                   (if constant ;; binding the same variable to two constants
                       (return-from compute-mapping (values nil nil))
                       (setf constant e))))
             (dolist (v variables)
               (setf (getf mapping v) constant)))
       (values mapping t)))))

(defun make-equivalence (&optional aliases)
  (let ((ec (%make-equivalence)))
    (iter (for (x . y) in aliases)
          (add-relation ec x y))
    ec))

;; (print (make-equivalence '((?x . ?y) (?y . ?z) (?z .  one) (?w . ?v))))

;; (print (compute-mapping
;;         (make-equivalence '((?x . ?y) (?y . ?z) (?z .  one) (?w . ?v)))))

;;; satisfiability

(defun satisfiable (aliases inequality)
  (apply #'map-product
         (lambda (&rest single-aliases)
           (when (test-aliases (reduce #'append single-aliases) inequality)
             (return-from satisfiable t)))
         aliases)
  nil)

;; list-only version
#+(or)
(defun test-aliases (aliases inequality)
  (let* ((elems nil)
         (elem-class nil)
         (class-elems nil)
         (mapping nil))
    ;; the implementation here is entirely based on a list and slow
    ;; should use a more efficient datastructure
    (iter (for (x . y) in aliases)
          (pushnew x elems)
          (pushnew y elems))
    (iter (for e in elems)
          (for i from 0)
          (setf (getf elem-class e) i)
          (setf (getf class-elems i) (list e)))
    ;; merge the classes
    (iter (for (x . y) in aliases)
          (for xc = (getf elem-class x))
          (for yc = (getf elem-class y))
          (when (> xc yc)
            (rotatef x y)
            (rotatef xc yc))
          ;; now xc < yc
          (setf (getf elem-class y) xc)
          (appendf (getf class-elems xc) (getf class-elems yc))
          (remf class-elems yc))
    ;; find the representative
    (iter (for (class elems . rest) on class-elems)
          (for constant = nil)
          (for variables = nil)
          (dolist (e elems)
            (if (variablep e)
                (push e variables)
                (if constant ;; binding the same variable to two constants
                    (return-from test-aliases nil)
                    (setf constant e))))
          (dolist (v variables)
            (setf (getf mapping v) constant)))
    ;; check if for all disjunctions, at least one clause is satisfied
    (every (lambda (disjunction)
             (some (lambda-ematch
                     ((cons x y)
                      (not (eq (getf mapping x)
                               (getf mapping y)))))
                   disjunction))
           inequality)))

(defun test-aliases (aliases inequality)
  (let ((ec (make-equivalence aliases)))
    (multiple-value-bind (mapping consistent-p) (compute-mapping ec)
      (unless consistent-p
        (return-from test-aliases nil))
      ;; check if for all disjunctions, at least one clause is satisfied
      (every (lambda (disjunction)
               (some (lambda-ematch
                      ((cons x y)
                       (not (eq (getf mapping x)
                                (getf mapping y)))))
                     disjunction))
             inequality))))
