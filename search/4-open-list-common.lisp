(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(define-condition no-solution (simple-error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c)) 
     (format s "Open list exhausted!"))))

