
(defpackage :strips.lib
  (:use :cl :trivia :iterate :alexandria)
  (:export
   #:make-index
   #:index
   #:index-id
   #:index-insert
   #:index-ref
   #:index-size
   #:make-trie
   #:trie-insert
   #:trie-member
   #:trie-insert-all
   #:map-trie
   #:query-trie))

(in-package :strips.lib)

(defmacro ftype* (name &rest types)
  `(declaim (ftype (function ,(butlast types) ,(lastcar types)) ,name)))
