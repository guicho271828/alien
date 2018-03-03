
(in-package :strips.lib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +t+ '+t+))

(ftype* make-trie list)
(defun make-trie ()
  "Trie implementation based on plist. not for performance"
  (list +t+ +t+))

(defmacro trie-insert (trie list)
  `(setf ,trie (%trie-insert ,trie ,list)))

(ftype* %trie-insert list list list)
(defun %trie-insert (trie list)
  (match list
    (nil
     (setf (getf trie +t+) +t+)
     trie)
    ((list* first rest)
     (let ((child (getf trie first)))
       (if child
           (progn
             (setf (getf trie first)
                   (%trie-insert child rest))
             trie)
           (progn
             (setf (getf trie first)
                   (%trie-insert nil rest))
             trie))))))

(ftype* trie-member list list (or (eql +t+) null))
(defun trie-member (trie list)
  (ematch list
    (nil
     (getf trie +t+))
    ((list* first rest)
     (trie-member (getf trie first) rest))))
  
(ftype* trie-insert-all list list list)
(defun trie-insert-all (trie lists)
  (iter (for list in lists)
        (setf trie (trie-insert trie list)))
  trie)

(ftype* map-trie (function (list) t) list null)
(defun map-trie (fn trie)
  "traverse over the trie and calls FN on each leaf"
  (let ((stack (make-array 32 :adjustable t :fill-pointer 0)))
    (labels ((rec (trie)
               (iter (for (key child . rest) on trie by #'cddr)
                     (if (eq +t+ key)
                         (funcall fn (coerce stack 'list))
                         (progn
                           (vector-push-extend key stack 32)
                           (rec child)
                           (vector-pop stack))))))
      (rec trie))
    nil))

(ftype* variablep symbol boolean)
(defun variablep (variable)
  (ematch variable
    ((symbol :name (string* #\? _))
     t)
    ((symbol)
     nil)))

;; WIP
;; The function is called with the matching symbol, and :binding keyword argument with &allow-other-keys.

(ftype* query-trie (function (list) t) list list null)
(defun query-trie (fn trie query)
  "Traverse over the trie matching the query, where query is a list containing symbols.
Symbols whose name starts from ? are regarded as variable."
  (assert (every #'symbolp query))
  (let ((stack (make-array 32 :adjustable t :fill-pointer 0)))
    (labels ((rec (trie query)
               (match query
                 (nil
                  (when (getf trie +t+)
                    (funcall fn (coerce stack 'list))))
                 ((list* head rest)
                  (if (variablep head)
                      (iter (for (key child . _) on trie by #'cddr)
                            (if (eq +t+ key)
                                nil ; insufficient length
                                (progn
                                  (vector-push-extend key stack 32)
                                  (rec child rest)
                                  (vector-pop stack))))
                      (progn
                        (vector-push-extend head stack 32)
                        (rec (getf trie head) rest)
                        (vector-pop stack)))))))
      (rec trie query)
      nil)))

#+(or)
(let ((trie nil))
  (setf trie (trie-insert trie '(a b c)))
  (print trie)
  (setf trie (trie-insert trie '(a b c)))
  (print trie)
  (setf trie (trie-insert trie '(a b d)))
  (setf trie (trie-insert trie '(a b e)))
  (setf trie (trie-insert trie '(a c d)))
  (print trie)
  (setf trie (trie-insert trie '(a c)))
  (terpri)
  (pprint-fill *standard-output* trie)
  (assert (print (trie-member trie '(a c))))
  (assert (not (print (trie-member trie '(a b)))))
  
  (map-trie #'print trie)
  (terpri)
  (query-trie #'print trie '(a ? d)))

  
