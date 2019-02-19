
(in-package :alien)

(defun timeout (time searcher)
  (ematch searcher
    ((searcher storage form)
     (make-searcher
      :storage storage
      :form `(lambda ()
               (bt:with-timeout (,time)
                 (funcall ,form)))))))
