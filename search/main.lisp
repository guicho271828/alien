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

(defun simulate-plan-with-val (domain problem plan-output-file &aux (*package* (find-package :alien.pddl)))
  (match (read-from-string (read-file-into-string problem))
    ((list 'define (list 'problem _)
           (list :domain _)
           (list* :objects _)
           (list* :init facts)
           (list* :goal _))

     ;; validate should be in the PATH
     (let ((output (uiop:run-program (list "validate" "-v" domain problem plan-output-file)
                                     :output :string
                                     :error-output t :ignore-error-status t)))
       (with-input-from-string (s output)
         (iter (for line in-stream s using #'read-line)
               (with facts = (remove 'not facts :key #'first))
               (match line
                 ((ppcre "Deleting(.*)" (read fact))
                  ;; (assert (member fact facts :test 'equal))
                  (removef facts fact :test 'equal))
                 ((ppcre "Adding(.*)" (read fact))
                  ;; (assert (not (member fact facts :test 'equal)))
                  (pushnew fact facts :test 'equal))
                 ((ppcre "Checking next happening")
                  (collect facts)
                  (setf facts (copy-list facts)))
                 ((ppcre "Plan executed successfully")
                  (collect facts)
                  (finish))
                 (_
                  (format *error-output* "~&Ignoring: ~a" line)))))))))

(defun val-simulate-main (&rest argv)
  (declare (ignorable argv))
  (setf *terminal-io* *error-output*)
  (log:config :sane)
  (ematch argv
    ((list domain problem plan-output-file trace-output-file)
     (let ((trace (simulate-plan-with-val domain problem plan-output-file)))
       (with-open-file (s trace-output-file :direction :output :if-does-not-exist :create :if-exists :supersede) 
         (iter (for state in trace)
               (let ((*package* (find-package :alien.pddl)))
                 (print (sort state #'fact<) s)))
         (fresh-line s))))
    
    ((list domain problem plan-output-file)
     (iter (for state in (simulate-plan-with-val domain problem plan-output-file))
           (let ((*package* (find-package :alien.pddl)))
             (print (sort state #'fact<))))
     (fresh-line))
    (_
     (format *error-output* "Usage: alien-simulate.ros domain problem plan-output-file~%")
     (format *error-output* "Got ARGV: ~a~%" argv)
     (format *error-output* "~{~30@a: ~a~%~}"
             (list "dynamic space size" (sb-ext:dynamic-space-size)
                   "lisp implementation type" (lisp-implementation-type)
                   "lisp implementation version" (lisp-implementation-version)
                   "machine instance" (machine-instance)
                   "machine type" (machine-type)
                   "machine version" (machine-version)
                   "software type" (software-type)
                   "software version" (software-version))))))


