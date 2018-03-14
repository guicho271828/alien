(in-package :strips)

#|

Compilation process:

instance-depdendent code should be compiled/loaded three times.

1. The first LOAD happens while dumping the image. It is intended to let the
   system knows about the builder functions such as EAGER, BUCKET-OPEN-LIST,
   FF. This can be done without runtime information.

2. The second LOAD happens in the runtime. It is intended to collect the
   definitions of the packed structures based on the runtime information such as the state size.
   After this phase, the global STATE-INFORMATION packed structure can be compiled.

3. The third LOAD is intended to recompile the functions based on the STATE-INFORMATION.
   Search is performed using the result of this final compilation.

|#

(defun run (search-engine)
  (ematch search-engine
    ((searcher storage form)
     ;; second LOAD
     (log:info "compiling instance-dependent code for packed structs")
     (let ((*features* (cons 'phase/packed-structs
                             (append *optional-features* *features*))))
       (recompile-instance-dependent-code))
     (log-milestone :secondary-compilation)
     ;; compile STATE-INFORMATION
     (let ((*package* (find-package :strips))
           ;; default value is 200, which consumes too much time for compilation
           (sb-ext:*inline-expansion-limit* 10))
       ;; because SYMBOLICATE interns in the current package
       (eval `(strips.lib:define-packed-struct state-information ,storage)))
     (log:info (eval '(size-of 'state-information)))
     (log:info *memory-limit*)
     (log:info (max-state-id))
     (log-milestone :state-information)
     ;; third LOAD
     (log:info "compiling instance-dependent code for functions")
     (let ((*features* (cons 'phase/full-compilation
                             (append *optional-features* *features*))))
       (recompile-instance-dependent-code))
     (log-milestone :tertiery-compilation)
     (log:info "compiling main function")
     (let ((main (compile nil form)))
       (log-milestone :main-function-compilation)
       (log:info "garbage collection")
       (sb-ext:gc :full t)
       (log-milestone :gc)
       (log:info "Search started")
       (unwind-protect
            (funcall main)
         (log-milestone :search)
         (log:info "Search finished"))))))

(defun solve-common (domain problem fn)
  (log:info "[0.000s] [+0.000s] STARTED")
  (log:info "Solving ~a" problem)
  (let* ((*start-time* (get-internal-real-time))
         (*last-milestone* *start-time*))
    (with-parsed-information5 (-<> (parse problem domain)
                                (prog1 arrow-macros:<> (log-milestone :parse))
                                easy-invariant
                                (prog1 arrow-macros:<> (log-milestone :easy-invariant))
                                ground
                                (prog1 arrow-macros:<> (log-milestone :ground))
                                mutex-invariant
                                (prog1 arrow-macros:<> (log-milestone :mutex-invariant))
                                instantiate
                                (prog1 arrow-macros:<> (log-milestone :intantiate)))

      (log:info "       facts: ~A" *fact-size*)
      (log:info "      axioms: ~A" (length *ground-axioms*))
      (log:info "         ops: ~A" *op-size*)
      (log:info "axiom layers: ~A" (length *instantiated-axiom-layers*))
      (unwind-protect
           (let (*optional-features*)
             (funcall fn))
        (log:info "Finished on ~a" problem)))))

(defun solve-once (domain problem fn)
  "Solve the problem, return the first solution"
  (solve-common domain problem
                (lambda ()
                  (handler-bind ((goal-found
                                  (lambda (c)
                                    (declare (ignore c))
                                    (return-from solve-once (retrieve-path)))))
                    (funcall fn)))))

(defun solve-once-to-file (domain problem plan-output-file fn)
  "Solve the problem, return the first solution"
  (solve-common domain problem
                (lambda ()
                  (handler-bind ((goal-found
                                  (lambda (c)
                                    (declare (ignore c))
                                    (output-plan plan-output-file)
                                    (return-from solve-once-to-file))))
                    (funcall fn)))))

