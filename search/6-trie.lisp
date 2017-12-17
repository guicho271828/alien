
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

;; since I am fed up with the invariant synthesis, I now use the plain binary representation...

(defun generate-action-trie (info)
  (with-parsed-information info
    (match info
      ((plist :facts facts :ops ops)
       
       ))))
