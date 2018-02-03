
(in-package :strips.lib)

;; I looked up various alternatives, but none satisfies my needs of storing and retrieving
;; certain data as compact as possible.

;; on SBCL:
;; Basic common lisp structure consumes additional 2 words for a single structure.
;; This multiplies the space usage when you make a huge number of small structures.
;; we need much more compact, C-like struct.

;; C-style struct

(lispn:define-namespace packed-struct-layout packed-struct-layout nil
                        "A namespace for the layout of the packed structure")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (packed-struct-layout (:constructor
                                    make-packed-struct-layout
                                    (&key
                                     name names defaults types
                                     &aux
                                     (sizes (mapcar #'size-of types))
                                     (offsets (compute-offset sizes))
                                     (aux (assert (equal names (remove-duplicates names)))))))
    (name nil :type symbol)
    names
    defaults
    types
    sizes
    offsets))

(defun size-of (type)
  (match type
    ((packed-struct-layout sizes)
     (reduce #'+ sizes))
    (_
     (ematch (introspect-environment:typexpand type)
       ((type-r:integer-subtype low high)
        (if (minusp low)
            (1+ (integer-length high))
            (integer-length high)))
       ((type-r:float-subtype high)
        (multiple-value-bind (signif expon) (integer-decode-float high)
          (+ (integer-length signif)
             (integer-length expon)
             1)))
       ((type-r:member-type members)
        (size-of `(integer ,(reduce #'min members)
                           ,(reduce #'max members))))
       ((type-r:array-subtype element-type dimensions)
        (* (size-of element-type) (reduce #'* (ensure-list dimensions))))))))

(defun compute-offset (sizes)
  (iter (for s in sizes)
        (collecting sum)
        (summing s into sum)))

(defun merge-packed-struct-layout (structs &key (name (gensym)) names defaults types)
  (let ((result (reduce #'merge-packed-struct-layout-2
                        (mapcar #'symbol-packed-struct-layout structs)
                        :initial-value
                        (make-packed-struct-layout :name (gensym)
                                                   :names names
                                                   :defaults defaults
                                                   :types types))))
    (setf (packed-struct-layout-name result) name)
    result))

(defun merge-packed-struct-layout-2 (struct1 struct2)
  (ematch* (struct1 struct2)
    (((packed-struct-layout :names names1 :defaults defaults1 :types types1)
      (packed-struct-layout :names names2 :defaults defaults2 :types types2))
     (make-packed-struct-layout
      :name (gensym)
      :names    (append names1 names2)
      :defaults (append defaults1 defaults2)
      :types    (append types1 types2)))))

(defmacro define-packed-struct (name (&rest supertypes) &body slots)
  (let ((slots (mapcar #'ensure-list slots)))
    `(progn (setf (symbol-packed-struct-layout ',name)
                  (merge-packed-struct-layout
                   '(,@supertypes)
                   :name ',name
                   :names ',(mapcar #'first slots)
                   :defaults ',(mapcar #'second slots)
                   :types ',(mapcar #'third slots)))
            ',name)))

;; test

(deftype scalar () '(unsigned-byte 32))

(deftype parent () '(unsigned-byte 32))

(deftype generator () '(unsigned-byte 32))

(deftype status () '(member 0 1 2 3))

(define-packed-struct test1 ()
  (scalar 0 scalar)
  (parent 0 parent)
  (generator 0 generator))

#+(or)
(define-packed-struct test2 ; should fail, duplicated slot names
  (scalar 0 scalar)
  (scalar 0 scalar))

(define-packed-struct state-info ()
  (state 0 (bit-vector 42))
  (status +new+ status)
  (parent 0 parent)
  (generator 0 generator))

(define-packed-struct g ()
  (g 0 scalar))

(merge-packed-struct-layout '(state-info g)
                            :name 'state-info+g)

(define-packed-struct state-info+g (state-info g)
  )
