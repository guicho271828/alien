
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *memory-limit* 1073741 ; 1GB
  "maximum amount of memory in kB")

(defvar *time-limit* 300
  "runtime limit in sec")

(defun solve-once (domain problem plan-output-file fn)
  "Solve the problem, return the first solution"
  (with-parsed-information5 (-> (parse problem domain)
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (handler-bind ((goal-found (lambda (c)
                                 (declare (ignore c))
                                 (ensure-directories-exist plan-output-file)
                                 (with-open-file (s plan-output-file :direction :output
                                                    :if-exists :supersede
                                                    :if-does-not-exist :create)
                                   (print-plan (retrieve-path) s))
                                 (return-from solve-once))))
      (funcall fn))))

;; TODO: solve-many with specifying N

(define-condition no-solution (simple-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c)) 
     (format s "Open list exhausted!"))))

(define-condition goal-found (error) ())
