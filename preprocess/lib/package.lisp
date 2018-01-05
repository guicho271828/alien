
(defpackage :strips.lib
  (:use :cl :trivia :iterate :alexandria)
  (:export
   #:make-index
   #:index-insert
   #:index
   #:index-ref
   #:index-size
   #:make-trie
   #:trie-insert
   #:trie-member
   #:trie-insert-all
   #:map-trie
   #:query-trie))

