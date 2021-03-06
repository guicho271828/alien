;;;; Autogenerated ASD file for system "ALIEN"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem alien
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LLGPL"
 :depends-on (:iterate
              :alexandria
              :trivia
              :trivia.ppcre
              :trivia.quasiquote
              :arrow-macros
              :cl-prolog2.bprolog
              :bordeaux-threads
              :lisp-namespace
              :introspect-environment
              :type-r
              :static-vectors
              :log4cl
              :priority-queue
              :cffi)
 :serial t
 :components ((:module "lib"
               :components ((:file "package")
                            (:file "debug")
                            (:file "equivalence")
                            (:file "indexed-entries")
                            (:file "packed-struct")
                            (:file "struct-of-array")
                            (:file "trie")))
              (:module "preprocess"
               :components ((:file "package")
                            (:file "util")
                            (:file "specials")
                            (:file "2-translate")
                            (:file "4-easy-invariant")
                            (:file "5-grounding-prolog-3")
                            (:file "6-invariant")
                            (:file "7-instantiate")
                            (:file "8-successor-generator")))
              (:module "search"
               :components ((:file "util")
                            (:file "specials")
                            (:file "blind")
                            (:file "bucket-open-list")
                            (:file "builders")
                            (:file "compile-effect")
                            (:file "compile-sg")
                            (:file "non-axiom-goals")
                            (:file "relaxation")
                            (:file "timeout")
                            (:file "run")
                            (:file "main")))
              (:module "validate"
               :components ((:file "validate"))))
 :description "A ALIEN planner"
 :in-order-to ((test-op (test-op :alien.test))))
