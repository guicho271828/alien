#|
  This file is a part of strips project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :strips.test
  (:use :cl
        :strips
        :fiveam
        :iterate :alexandria :trivia))
(in-package :strips.test)



(def-suite :strips)
(in-suite :strips)

;; run test with (run! test-name) 

(test strips

  )



