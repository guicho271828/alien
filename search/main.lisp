(in-package :alien)

(defun alien-main (&rest argv)
  (declare (ignorable argv))
  (setf *terminal-io* *error-output*)
  (log:config :sane)
  (ematch argv
    ((list* "-t" (read *time-limit*) rest)
     (apply #'alien-main rest))
    ((list* "-m" (read *memory-limit*) rest)
     (apply #'alien-main rest))
    ((list* "--sg-compiled-branch-limit" (read *sg-compiled-branch-limit*) rest)
     (apply #'alien-main rest))
    ((list* "--search-option" (read *search-option*) rest)
     (apply #'alien-main rest))
    ((list* "--alias" (read alias) rest)
     (apply #'alien-main
            "--search-option"
            (princ-to-string (second (assoc alias *aliases*)))
            rest))
    
    ((list problem)
     (print-plan
      (solve-once
       (find-domain problem) problem #'main-search))
     (fresh-line))
    
    ((list domain problem)
     (print-plan
      (solve-once domain problem #'main-search))
     (fresh-line))
    
    ((list domain problem plan-output-file)
     (solve-once-to-file
      domain problem plan-output-file #'main-search)
     (fresh-line))
    (_
     (format *error-output* "Usage: alien.ros [options] [domain] problem [plan-output-file]~%")
     (format *error-output* "Got ARGV: ~a~%" argv)
     (format *error-output* "~{~30@a: ~a~%~}"
             (list "dynamic space size" (sb-ext:dynamic-space-size)
                   "lisp implementation type" (lisp-implementation-type)
                   "lisp implementation version" (lisp-implementation-version)
                   "machine instance" (machine-instance)
                   "machine type" (machine-type)
                   "machine version" (machine-version)
                   "software type" (software-type)
                   "software version" (software-version)))
     (show-doc
      `((-t sec *time-limit*)
        (-m mbyte *memory-limit*)
        (--sg-compiled-branch-limit limit *sg-compiled-branch-limit*)
        (--search-option SEXP *search-option*)
        (--alias NAME *aliases*))))))

