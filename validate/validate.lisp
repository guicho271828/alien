
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

(defun validator ()
  (fd-relative-pathname*
   "builds/release64/bin/validate"
   "builds/release32/bin/validate"
   "src/validate"
   "validate"))

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
                    (when ,debug
                      (format *error-output* "~&; ~a~%" ,command))
                    ,command)
                  :output '(:string :stripped t))))
       (unwind-protect
            (progn ,@body)
         (if ,debug
             (format *error-output* "~&; not removing ~a for debugging~%" ,var)
             (uiop:run-program (format nil "rm -rf ~a" (namestring ,var)) :ignore-error-status t))))))

(defgeneric validate-plan (domain problem plan &key verbose))

(defmethod validate-plan (domain problem (plan list) &key verbose)
  (with-temp (planfile :debug verbose)
    (with-open-file (s planfile :direction :output :if-exists :supersede)
      (print-plan plan s))
    (validate-plan domain problem planfile :verbose verbose)))

(defmethod validate-plan (domain (problem list) plan &key verbose)
  (with-temp (problemfile :debug verbose)
    (with-open-file (s problemfile :direction :output :if-exists :supersede)
      (format s "~:a" problem))
    (validate-plan domain problemfile plan :verbose verbose)))

(defmethod validate-plan ((domain list) problem plan &key verbose)
  (with-temp (domainfile :debug verbose)
    (with-open-file (s domainfile :direction :output :if-exists :supersede)
      (format s "~:a" domain))
    (validate-plan domainfile problem plan :verbose verbose)))

(defmethod validate-plan (domain problem (plan string) &key verbose)
  (validate-plan domain problem (pathname plan) :verbose verbose))

(defmethod validate-plan (domain (problem string) plan &key verbose)
  (validate-plan domain (pathname problem) plan :verbose verbose))

(defmethod validate-plan ((domain string) problem plan &key verbose)
  (validate-plan (pathname domain) problem plan :verbose verbose))

(defmethod validate-plan ((domain pathname)
                          (problem pathname)
                          (plan pathname)
                          &key
                            verbose)
  (assert (probe-file domain))
  (assert (probe-file problem))
  (assert (probe-file plan))
  (let* ((command (format nil "~a ~:[~;-v~] ~a ~a ~a"
                          (or (validator)
                              (return-from validate-plan t))
                          verbose domain problem plan)))
    (when verbose (format t "~&; ~a~%" command))
    (zerop (nth-value 2 (uiop:run-program command
                                          :output (if verbose t nil)
                                          :error-output t :ignore-error-status t)))))


