
(in-package :strips)

(defun timeout (searcher)
  (ematch searcher
    ((searcher storage form)
     (make-searcher
      :storage storage
      :form `(lambda ()
               (bt:with-timeout (*time-limit*)
                 (funcall ,form)))))))
