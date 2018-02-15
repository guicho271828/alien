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
     (let ((*features* (cons 'phase/packed-structs *features*)))
       (recompile-instance-dependent-code))
     ;; compile STATE-INFORMATION
     (let ((*package* (find-package :strips)))
       ;; because SYMBOLICATE interns in the current package
       (eval `(strips.lib:define-packed-struct state-information (,storage))))
     ;; third LOAD
     (let ((*features* (cons 'phase/full-compilation *features*)))
       (recompile-instance-dependent-code))
     (funcall (compile nil form)))))
