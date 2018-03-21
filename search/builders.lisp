

(in-package :strips)

(defun sum (&rest evaluators)
  (with-gensyms (sum)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,sum ()
                       (value 0 (runtime integer 0
                                         (reduce #'+
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key (lambda (struct) (expt 2 (size-of struct))))))))
     :function `(lambda (state)
                  (+ ,@(mapcar
                        (lambda (e)
                          `(funcall ,(evaluator-function e) state))
                        evaluators))))))

(defun product (&rest evaluators)
  (with-gensyms (product)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,product ()
                       (value 0 (runtime unsigned-byte
                                         ;; because the size of storages are unknown when
                                         ;; the builders are evaluated
                                         (reduce #'+
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key #'size-of)))))
     :function `(lambda (state)
                  (* ,@(mapcar
                        (lambda (e)
                          `(funcall ,(evaluator-function e) state))
                        evaluators))))))

(defun maximum (&rest evaluators)
  (with-gensyms (maximum)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,maximum ()
                       (value 0 (runtime unsigned-byte
                                         (reduce #'max
                                                 (append ,@(mapcar #'evaluator-storage evaluators))
                                                 :key #'size-of
                                                 :initial-value 0)))))
     :function `(lambda (state)
                  (max ,@(mapcar
                          (lambda (e)
                            `(funcall ,(evaluator-function e) state))
                          evaluators))))))

(defun constant (value)
  (assert (integerp value))
  (with-gensyms (constant)
    (make-evaluator
     :storage `(list (strips.lib:define-packed-struct ,constant ()
                       (value 0 (integer ,value ,value))))
     :function `(constantly ,value))))

(defun tiebreak (evaluator &rest more)
  (if more
      (let ((lower-priority (apply #'tiebreak more)))
        (with-gensyms (tiebreak)
          (make-evaluator
           :storage `(list (strips.lib:define-packed-struct ,tiebreak ()
                             (value 0 (runtime unsigned-byte
                                               (+ (size-of (first ,(evaluator-storage evaluator)))
                                                  (size-of (first ,(evaluator-storage lower-priority))))))))
           :function `(lambda (state)
                        (+ (ash (funcall ,(evaluator-function evaluator) state)
                                (maybe-inline-obj
                                 (size-of (first ,(evaluator-storage lower-priority)))))
                           (funcall ,(evaluator-function lower-priority) state))))))
      evaluator))

(defun threshold (threshold evaluator &key (except-init t))
  (assert (integerp threshold))
  (make-evaluator
   :storage (evaluator-storage evaluator)
   :function `(lambda (state)
                (let ((value (funcall ,(evaluator-function evaluator) state))
                      ,@(when except-init `((init t))))
                  (if (and ,@(when except-init `((not init))) (< ,threshold value))
                      (throw 'prune t)
                      (progn
                        ,@(when except-init `((setf init nil)))
                        value))))))

(defmacro evaluator-let (bindings &body body)
  `(call-with-temporary-variables
    (list ,@(mapcar #'second bindings))
    (lambda ,(mapcar #'first bindings)
      ,@body)))

(defun call-with-temporary-variables (evaluators fn)
  "Convert the given evaluators to evaluators which just reference the cached values in lexical variables.
EVALUATORS: a list of evaluators.
FN: a function which takes as many arguments as the length of evaluators.
This function is called with evaluators each of which just refers to a lexical variable
bound to the result of evaluation.

Equivalent to FD's evaluation context / heuristic cache, but implemented much conveniently.
"
  (let* ((temporaries
          (mapcar (lambda (tmp)
                    (gensym "TMP"))
                  evaluators))
         (locally-cached-evaluators
          (mapcar (lambda-ematch*
                    (((evaluator storage) tmp)
                     ;; Evaluation of this evaluator inside FN
                     ;; is merely an access to a local variable.
                     (make-evaluator
                      :storage storage
                      :function `(lambda (state) (declare (ignore state)) ,tmp))))
                  evaluators
                  temporaries))
         (result
          (apply fn locally-cached-evaluators)))
    (ematch result
      ((evaluator storage function)
       (make-evaluator
        :storage storage
        :function `(lambda (state)
                     (let ,(mapcar (lambda-ematch*
                                     (((evaluator function) tmp)
                                      ;; create a lexical binding
                                      `(,tmp (funcall ,function state))))
                                   evaluators
                                   temporaries)
                       (funcall ,function state))))))))

#| Usage:

(evaluator-let ((h (ff/rpg)))
  (tiebreak
   h
   (novelty-for h)))

=>

(call-with-temporary-variables
 (list (ff/rpg))
 (lambda (h)
   (tiebreak
    h
    (novelty-for h))))

=> produces code below

(lambda (state)
  (let ((#:TMP0 (funcall (function ff-heuristics/rpg) state))) ; expansion for ff/rpg evaluator
    
    ;; expansion for TIEBREAK evaluator
    (funcall (lambda (state)
               (+ (ash (funcall
                        ;; expansion for the locally cached ff/rpg evaluator
                        (lambda (state) (declare (ignore state)) #:TMP0)
                        state)
                       (maybe-inline-obj
                        (size-of (first ...))))
                  (funcall (lambda (state)
                             ;; expansion for NOVELTY-FOR evaluator
                             ...)
                           state)))
             state)))
|#
