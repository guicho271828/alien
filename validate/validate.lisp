
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defun search-fd ()
  (labels ((rec (path)
             (let ((downward (merge-pathnames "downward/" path)))
               (if (probe-file downward)
                   downward
                   (let ((parent (truename (merge-pathnames "../" path))))
                     (if (equal '(:absolute) (pathname-directory parent))
                         (warn "Fast Downward was not found!")
                         (rec parent)))))))
    (rec
     (asdf:system-source-directory :strips))))

(defun fd-relative-pathname (path)
  (let ((path (merge-pathnames
               path
               (truename
                (or (uiop:getenv "FD_DIR")
                    (search-fd))))))
    (assert (probe-file path))
    path))

(defun fd-relative-pathname* (&rest paths)
  "returns the first match"
  (or (iter (for path in paths)
            (handler-case
                (leave (fd-relative-pathname path))
              (error ())))
      (warn "validator not found!")))

;; we now uses this
(defun search-val ()
  (labels ((rec (path)
             (let ((downward (merge-pathnames "VAL/" path)))
               (if (probe-file downward)
                   downward
                   (let ((parent (truename (merge-pathnames "../" path))))
                     (if (equal '(:absolute) (pathname-directory parent))
                         (warn "VAL was not found!")
                         (rec parent)))))))
    (rec
     (asdf:system-source-directory :strips))))

(defun validator ()
  (merge-pathnames "validate" (search-val)))

(defun print-plan (plan &optional (stream *standard-output*))
  (dolist (action plan)
    (write action :stream stream :escape nil :case :downcase :level nil :length nil)
    (terpri stream)))

(defmacro with-temp ((var &key directory template (tmpdir "/tmp/") debug) &body body)
  "Create a temporary file, then remove the file by unwind-protect.
Most arguments are analogous to mktemp.
When DIRECTORY is non-nil, creates a directory instead.
DEBUG is a form.
When evaluated to non-nil, it does not remove the directory so that you can investigate what happened inside the directory.
It may be evaluated multiple times."
  (with-gensyms (command)
    `(let ((,var (uiop:run-program
                  (let ((,command (format nil "mktemp --tmpdir='~a' ~@[-d~*~] ~@[~a~]"
                                          ,tmpdir ,directory ,template)))
                    (log:debug "~a" ,command)
                    ,command)
                  :output '(:string :stripped t))))
       (unwind-protect
            (progn ,@body)
         (if ,debug
             (log:debug "not removing ~a for debugging" ,var)
             (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t))))))

(defgeneric validate-plan (domain problem plan))

(defmethod validate-plan (domain problem (plan list))
  (with-temp (planfile :debug t)
    (with-open-file (s planfile :direction :output :if-exists :supersede)
      (print-plan plan s))
    (validate-plan domain problem planfile)))

(defmethod validate-plan (domain (problem list) plan)
  (with-temp (problemfile :debug t)
    (with-open-file (s problemfile :direction :output :if-exists :supersede)
      (format s "~:a" problem))
    (validate-plan domain problemfile plan)))

(defmethod validate-plan ((domain list) problem plan)
  (with-temp (domainfile :debug t)
    (with-open-file (s domainfile :direction :output :if-exists :supersede)
      (format s "~:a" domain))
    (validate-plan domainfile problem plan)))

(defmethod validate-plan (domain problem (plan string))
  (validate-plan domain problem (pathname plan)))

(defmethod validate-plan (domain (problem string) plan)
  (validate-plan domain (pathname problem) plan))

(defmethod validate-plan ((domain string) problem plan)
  (validate-plan (pathname domain) problem plan))

(defmethod validate-plan ((domain pathname)
                          (problem pathname)
                          (plan pathname))
  (assert (probe-file domain))
  (assert (probe-file problem))
  (assert (probe-file plan))
  (let ((validator (validator)))
    (if validator
        (log:debug "validator found: ~a" validator)
        (progn (log:warn "validator not found, validation is treating as a success")
               (return-from validate-plan t)))
    (let ((command (format nil "~a ~:[~;-v~] ~a ~a ~a"
                           validator t domain problem plan)))
      (log:debug command)
      (multiple-value-bind (output error status)
          (uiop:run-program command
                            :output :string
                            :error-output t :ignore-error-status t)
        (log:info output)
        (zerop status)))))


