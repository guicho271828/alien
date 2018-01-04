
(defpackage :strips.lib
  (:use :cl :trivia :iterate :alexandria)
  (:export
   #:make-index
   #:index-insert
   #:index
   #:index-ref
   #:index-size))

