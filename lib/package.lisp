
(defpackage :alien.lib
  (:use :cl :trivia :iterate :alexandria :static-vectors)
  (:export
   #:make-index
   #:index
   #:index-id
   #:index-insert
   #:index-ref
   #:index-size
   #:index-array
   #:index-hash
   #:make-trie
   #:trie-insert
   #:trie-member
   #:trie-insert-all
   #:map-trie
   #:query-trie
   #:defstruct-of-array
   #:define-packed-struct
   #:packed-aref
   #:packed-struct-layout
   #:packed-struct-layout-name
   #:packed-struct-layout-default-by-name
   #:packed-struct-layout-type-by-name
   #:packed-struct-layout-size-by-name
   #:packed-struct-layout-offset-by-name
   #:unbound-packed-struct-layout
   #:symbol-packed-struct-layout
   #:packed-struct-layout-boundp
   #:merge-packed-struct-layout
   #:size-of))

(in-package :alien.lib)

(defmacro ftype* (name &rest types)
  `(declaim (ftype (function ,(butlast types) ,(lastcar types)) ,name)))
