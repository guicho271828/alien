
(in-package :strips.lib)

;; hackish plist-based trie implementation
;; not for performance

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +terminal+ '+terminal+))

(defun trie-push (plist list)
  (match list
    (nil
     (setf (getf plist +terminal+) +terminal+)
     plist)
    ((list* first rest)
     (let ((child (getf plist first)))
       (if child
           (progn
             (setf (getf plist first)
                   (trie-push child rest))
             plist)
           (progn
             (setf (getf plist first)
                   (trie-push nil rest))
             plist))))))

(defun trie-member (plist list)
  (match list
    (nil
     (getf plist +terminal+))
    ((list* first rest)
     (trie-member (getf plist first) rest))))
  
(defun trie-push-all (plist lists)
  (iter (for list in lists)
        (setf plist (trie-push plist list)))
  plist)

(defun map-trie (fn plist)
  (let ((stack (make-array 32 :adjustable t :fill-pointer 0)))
    (labels ((rec (plist)
               (iter (for (key child . rest) on plist by #'cddr)
                     (if (eq +terminal+ key)
                         (funcall fn (coerce stack 'list))
                         (progn
                           (vector-push-extend key stack 32)
                           (rec child)
                           (vector-pop stack))))))
      (rec plist))))

#+(or)
(let ((trie nil))
  (setf trie (trie-push trie '(a b c)))
  (print trie)
  (setf trie (trie-push trie '(a b c)))
  (print trie)
  (setf trie (trie-push trie '(a b d)))
  (setf trie (trie-push trie '(a b e)))
  (setf trie (trie-push trie '(a c d)))
  (print trie)
  (setf trie (trie-push trie '(a c)))
  (terpri)
  (pprint-fill *standard-output* trie)
  (assert (print (trie-member trie '(a c))))
  (assert (not (print (trie-member trie '(a b)))))
  
  (map-trie #'print trie))

  
