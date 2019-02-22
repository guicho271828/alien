
;; unused

(in-package :alien.lib)

(defparameter *soa-initial-size* 32)

(defmacro map-slotdefs (((slotname default type) slots) &body forms)
  `(mapcar (flet ((fn (,slotname ,default ,type)
                    (declare (ignorable ,slotname ,default ,type))
                    ,@forms))
             (lambda (slot)
               (ematch slot
                 ((symbol)
                  (fn slot nil t))
                 ((list slotname default)
                  (fn slotname default t))
                 ((list slotname default :type type)
                  (fn slotname default type)))))
           ,slots))

(defmacro defstruct-of-array (name &body slots)
  `(progn
     (defstruct (,name (:conc-name ,(symbolicate '% name '-))
                       (:constructor ,(symbolicate 'make- name '-array)))
       (%count 0 :type array-index)
       (%total-size *soa-initial-size* :type array-index)
       ,@(map-slotdefs ((slotname default type) slots)
           `(,slotname (make-array *soa-initial-size*
                                   :initial-element ,default
                                   :element-type ',type))))
     (defun ,(symbolicate 'make- name) (soa-obj &key
                                                  ,@(map-slotdefs ((slotname default type) slots)
                                                      `(,slotname ,default)))
       ,(with-gensyms (c c2 size size2)
          `(ematch soa-obj
             ((,name :%count (place ,c ,c2) :%total-size (place ,size ,size2))
              (prog1 ,c2
                (when (= ,c2 ,size2)
                  (setf ,size2 (* 2 ,size2)
                        ,size ,size2)
                  ,@(map-slotdefs ((slotname default type) slots)
                      `(let ((new-array (make-array ,size2
                                                    :initial-element ,default
                                                    :element-type ',type)))
                         (replace new-array (,(symbolicate '% name '- slotname) soa-obj))
                         (setf (,(symbolicate '% name '- slotname) soa-obj) new-array))))
                ,@(map-slotdefs ((slotname default type) slots)
                    `(setf
                      (aref (,(symbolicate '% name '- slotname) soa-obj) ,c2)
                      ,slotname))
                (incf ,c))))))

     ,@(map-slotdefs ((slotname default type) slots)
         `(ftype* ,(symbolicate name '- slotname) ,name array-index ,type))
     ,@(map-slotdefs ((slotname default type) slots)
         `(defun ,(symbolicate name '- slotname) (soa-obj id)
            (aref (,(symbolicate '% name '- slotname) soa-obj) id)))))

#+(or)
(defstruct-of-array x
  a
  (b 0)
  (c 0 :type fixnum))

#+(or)
(let ((*x* (make-x-array)))
  (print *x*)
  (dotimes (i 32)
    (make-x *x* :a :hi! :b 4 :c 4))
  (print *x*)
  (print (make-x *x* :a :hi! :b 4 :c 4))
  (print *x*))

