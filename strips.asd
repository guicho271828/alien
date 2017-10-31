(defsystem strips
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :alexandria :trivia)
  :pathname "src"
  :components ((:file "package"))
  :description "A STRIPS planner"
  :in-order-to ((test-op (test-op :strips.test))))
