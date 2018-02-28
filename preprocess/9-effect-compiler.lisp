(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun find-lexical-variables (env)
  (mapcar #'car
          (sb-c::lexenv-vars
           (sb-c::coerce-to-lexenv env))))

(defmacro fcase9 (i &body body &environment env)
  "Jump-table based CASE implementation by myself
See https://gist.github.com/guicho271828/707be5ad51edb858ff751d954e37c267 for summary"
  (let* ((vars (find-lexical-variables env))
         (types (mapcar (rcurry #'introspect-environment:variable-type env) vars)))
    `(funcall
      (the (function ,types *)
           (svref (the (simple-vector ,(length body))
                       (load-time-value
                        (vector
                         ,@(iter (for b in body)
                                 (for j from 0)
                                 (collecting
                                  `(lambda (,@vars) (locally (declare ((eql ,j) ,i)) ,b)))))
                        t))
                  ,i))
      ,@vars)))

;; Example
#+(or)
(defun 256way/fcase9 (i)
  (let ((rand (random 10)))
    (fcase9 i
      . #.(loop :for x :from 0 :repeat 256
             :collect `(progn (* i rand))))))

(defparameter *effect-compilation-threashold* 3000)
(defmacro compiled-apply-op (op-id state child ops)
  (assert (symbolp ops))
  (if (< (length *instantiated-ops*) *effect-compilation-threashold*)
      (%compiled-apply-op op-id state child ops)
      (%interpret-apply-op op-id state child ops)))

(defun %compiled-apply-op (op-id state child ops)
  `(progn
     (fcase9 ,op-id
       ,@(iter (for op in-vector (symbol-value ops))
               (collecting
                `(progn
                   ,@(compile-apply-op op state child)))))
     ,child))

(defun compile-apply-op (op state child)
  (ematch op
    ((op eff)
     (iter (for e in-vector eff)
           (collecting
            (compile-apply-effect e state child))))))

(defun compile-apply-effect (effect state child)
  (ematch effect
    ((effect con eff)
     (let ((effect-form
            (if (minusp eff)
                `(setf (aref ,child ,(lognot eff)) 0)
                `(setf (aref ,child ,eff) 1)))
           (condition-form
            `(and ,@(iter (for i in-vector con)
                          (collecting
                           (if (minusp i)
                               `(= 0 (aref ,state ,(lognot i)))
                               `(= 1 (aref ,state ,i))))))))
       (if (zerop (length con))
           effect-form
           `(when ,condition-form
              ,effect-form))))))

(defun %interpret-apply-op (op-id state child ops)
  (log:warn "falling back to the interpretation based apply-op")
  `(progn
     (ematch (aref (load-time-value ,ops t) ,op-id)
       ((op eff)
        (iter (for e in-vector eff)
              (apply-effect e ,state ,child))))
     ,child))
