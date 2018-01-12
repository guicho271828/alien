;;;; Autogenerated ASD file for system "STRIPS"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem strips
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LLGPL"
 :depends-on (:iterate
              :alexandria
              :trivia
              :trivia.quasiquote
              :arrow-macros
              :cl-prolog2.swi
              :cl-prolog2.bprolog
              :bordeaux-threads)
 :serial t
 :components ((:module "preprocess"
               :components ((:file "package")
                            (:file "util")
                            (:file "specials")
                            (:module "lib"
                             :components ((:file "package")
                                          (:file "equivalence")
                                          (:file "indexed-entries")
                                          (:file "trie")))
                            (:file "2-translate")
                            (:file "4-easy-invariant")
                            (:file "5-grounding-prolog-3")
                            (:file "6-invariant")
                            (:file "7-instantiate")
                            (:file "8-successor-generator")))
              (:module "search"
               :components ((:file "0-special")
                            (:file "0-util")
                            (:file "1-state")
                            (:file "2-search-common")
                            (:file "3-heuristic-common")
                            (:file "4-open-list-common")
                            (:module "heuristics"
                             :components ())
                            (:module "searchers"
                             :components ((:file "eager")
                                          (:file "timeout")))
                            (:module "open-list"
                             :components ((:file "bucket-open-list"))))))
 :description "A STRIPS planner"
 :in-order-to ((test-op (test-op :strips.test))))
