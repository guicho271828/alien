
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *memory-limit* 1073741 ; 1GB
  "maximum amount of memory in kB")

(defvar *time-limit* 300
  "runtime limit in sec")

(defun recompile-instance-dependent-code ()
  (setf sb-ext:*inline-expansion-limit* 10)
  (asdf:compile-system :strips.instance-dependent :force t)
  (asdf:load-system :strips.instance-dependent :force t)
  ;; ensure the specialised code is removed and does not affect the later debugging
  (asdf:clear-system :strips.instance-dependent))

(defun output-plan (plan-output-file)
  "write the plan into a file"
  (ensure-directories-exist plan-output-file)
  (with-open-file (s plan-output-file :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print-plan (retrieve-path) s)))

(defun solve-once (domain problem fn)
  "Solve the problem, return the first solution"
  (with-parsed-information5 (-> (parse problem domain)
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (handler-bind ((goal-found
                    (lambda (c)
                      (declare (ignore c))
                      (return-from solve-once (retrieve-path)))))
      (funcall fn))))

(defun solve-once-to-file (domain problem plan-output-file fn)
  "Solve the problem, return the first solution"
  (with-parsed-information5 (-> (parse problem domain)
                              easy-invariant
                              ground
                              mutex-invariant
                              instantiate)
    (recompile-instance-dependent-code)
    (handler-bind ((goal-found
                    (lambda (c)
                      (declare (ignore c))
                      (output-plan plan-output-file)
                      (return-from solve-once-to-file))))
      (funcall fn))))

;; TODO: solve-many with specifying N

(define-condition no-solution (simple-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c)) 
     (format s "Open list exhausted!"))))

(define-condition goal-found (error) ())

(defun retrieve-path () (invoke-restart (find-restart 'retrieve-path)))

(enumerate status +new+ +open+ +closed+ +dominated+)

(defstruct builder storage)

(defstruct (evaluator (:include builder)) function)

(defstruct (open-list (:include builder)) constructor insert pop)

(defstruct (searcher (:include builder)) form)
