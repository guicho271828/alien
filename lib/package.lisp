
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
   #:query-trie
   #:defstruct-of-array
   #:define-packed-struct
   #:packed-aref
   #:unbound-packed-struct-layout
   #:symbol-packed-struct-layout
   #:packed-struct-layout-boundp
   #:merge-packed-struct-layout))

(in-package :strips.lib)

(defmacro ftype* (name &rest types)
  `(declaim (ftype (function ,(butlast types) ,(lastcar types)) ,name)))
