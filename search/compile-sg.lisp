
(in-package :strips)
(named-readtables:in-readtable :fare-quasiquote)

(defvar *sg-compiled-branch-limit* 1000) ; this roughly keeps the generator within 20k bytes
(defvar *sg-compiled-depth-limit*)

(defun find-depth-limit (sg branch-limit)
  (when (listp sg)
    (return-from find-depth-limit *state-size*))
  (let ((max-var 0)
        (branches 0)
        (q (pqueue:make-pqueue #'<)))
    (pqueue:pqueue-push sg (sg-node-variable sg) q)
    (iter (until (pqueue:pqueue-empty-p q))
          (for key = (pqueue:pqueue-front-key q))
          (for pkey previous key)
          (when pkey
            (when (< pkey key)
              (setf max-var pkey)))
          
          (for sg = (pqueue:pqueue-pop q))
          (incf branches)
          (when (< branch-limit branches)
            (return-from find-depth-limit max-var))
            
          (ematch sg
            ((sg-node variable then else either)
             (unless (listp then)
               (pqueue:pqueue-push then (sg-node-variable then) q))
             (unless (listp else)
               (pqueue:pqueue-push else (sg-node-variable else) q))
             (unless (listp either)
               (pqueue:pqueue-push either (sg-node-variable either) q)))
            ((list* op-ids)
             ;; nothing
             ))
          (finally
           (return *state-size*)))))

(defmacro do-leaf ((op-id state sg) &body body &environment env)
  (assert (symbolp state))
  (assert (symbolp op-id))
  (assert (symbolp sg))
  (log:info "compiling a successor generator")
  (let ((*sg-compiled-depth-limit*
         (find-depth-limit (symbol-value sg) *sg-compiled-branch-limit*)))
    (log:info "compiled branch limit: ~a" *sg-compiled-branch-limit*)
    (log:info "maximum depth to compile sg: ~a" *sg-compiled-depth-limit*)
    (prog1
        `(labels ((interpret (node)
                    (ematch node
                      ((type list)
                       (dolist (,op-id node)
                         (declare (op-id ,op-id))
                         ,@body))
                      ((sg-node variable then else either)
                       (if (= 1 (aref ,state variable))
                           (interpret then)
                           (interpret else))
                       (interpret either)))))
           ,(compile-iteration-over-leaf op-id state (symbol-value sg) body))
      (log:info "done"))))

(defun compile-iteration-over-leaf (op-id-sym state-sym sg body
                                    &optional (start 0)
                                    &aux
                                      (end (min *state-size* (+ start 64)))
                                      (width (- end start)))
  "Returns a program that iterates over the leaf of sg, inlining constants, and execute BODY on each loop."
  (with-gensyms (pack)
    (labels ((rec (sg binding)
               (ematch sg
                 ((sg-node variable then else either)
                  (if (<= variable *sg-compiled-depth-limit*)
                      (if (< variable end)
                          (if (and then else)
                              `((if (= 1 (aref ,state-sym ,variable))
                                    ;; already checked this variable, so no longer to extend the binding
                                    (progn ,@(rec then binding))
                                    (progn ,@(rec else binding)))
                                ,@(rec either binding))
                              (append (rec then (cons (cons variable t) binding))
                                      (rec else (cons (cons variable nil) binding))
                                      (rec either binding)))
                          (wrap-check
                           binding
                           (lambda ()
                             (list (compile-iteration-over-leaf op-id-sym state-sym sg body end)))))
                      (wrap-check
                       binding
                       (lambda ()
                         `((interpret ,sg))))))
                 ((list* op-ids)
                  (wrap-check
                   binding
                   (lambda ()
                     (when op-ids
                       (if (< (length op-ids) 4)
                           (iter (for id in op-ids)
                                 (appending
                                  (subst id op-id-sym body))) 
                           (with-gensyms (i)
                             `((dotimes (,i ,(length op-ids))
                                 (let ((,op-id-sym (aref (load-time-value
                                                          (make-array ,(length op-ids)
                                                                      :element-type 'op-id
                                                                      :initial-contents ',op-ids)
                                                          t)
                                                         ,i)))
                                   ,@body)))))))))))
             (wrap-check (binding cont)
               (when-let ((branches (funcall cont)))
                 (if binding
                     (if (second binding)
                         (let ((mask 0) (compare 0) (negative-condition nil))
                           ;; pack 64bit masked comparison
                           (iter (for (var . val) in binding)
                                 (for offset = (- var start))
                                 (setf (ldb (byte 1 offset) mask) 1)
                                 (if val
                                     (setf (ldb (byte 1 offset) compare) 1)
                                     (setf negative-condition t)))
                           (if negative-condition
                               ;; see note below
                               `((when (= 0 (logand ,mask (logxor ,compare ,pack)))
                                   ,@branches))
                               `((when (= 0 (logand ,compare (lognot ,pack)))
                                   ,@branches))))
                         (ematch binding
                           ((list (cons var val))
                            ;; special case for a single condition, see note 2 below
                            `((,(if val 'when 'unless) (logbitp ,(- var start) ,pack)
                                ,@branches)))))
                     branches))))
      `(let ((,pack (strips.lib::%packed-accessor-int ,state-sym ,width ,start)))
         ,@(rec sg nil)))))

;; note : logand+lognot saves additional storage for mask

;; (defun fn (x) (declare ((unsigned-byte 64) x)) (= 0 (LOGAND 35459518431232 (LOGNOT x))))
;; 
;; disassembly for FN
;; Size: 50 bytes. Origin: #x1001F40F64
;; ...
;; 6D:       488D141B         LEA RDX, [RBX+RBX]
;; 71:       4883F2FE         XOR RDX, -2
;; 75:       48231554FFFFFF   AND RDX, [RIP-172]               ; [#x1001F40ED0] = #x408020000000
;; 7C:       4885D2           TEST RDX, RDX
;; ...

;; (defun fn2 (x) (declare ((unsigned-byte 64) x)) (= 0 (logand 35459518431232 (LOGxor 35459518431232 x))))
;; 
;; disassembly for FN2
;; Size: 53 bytes. Origin: #x1001F40E78
;; 81:       488D141B         LEA RDX, [RBX+RBX]
;; 85:       48331554FFFFFF   XOR RDX, [RIP-172]               ; [#x1001F40DE0] = #x408020000000
;; 8C:       4823154DFFFFFF   AND RDX, [RIP-179]               ; [#x1001F40DE0] = #x408020000000
;; 93:       4885D2           TEST RDX, RDX
;; ...

;; note 2, when testing a single condition, logbitp is more compact

;; 52B6: L631: 48D1E1           SHL RCX, 1
;; 52B9:       4883F1FE         XOR RCX, -2
;; 52BD:       48230D5CC4FFFF   AND RCX, [RIP-15268]           ;; [#x225D1720] = #x41000000000
;; 52C4:       4885C9           TEST RCX, RCX
;; 52C7:       0F8444010000 JEQ L649

;; 3+4+7+3+6 = 23 byte / condition

;; 52CD: L632: 8B4805           MOV ECX, [RAX+5]
;; 52D0:       C1E909           SHR ECX, 9
;; 52D3:       83E102           AND ECX, 2
;; 52D6:       4883F902         CMP RCX, 2
;; 52DA:       0F841E010000     JEQ L648

;; 3+3+4+6 = 16 byte / condition

;; 9E1E: L632: 480FBAE12A       BT RCX, 42
;; 9E23:       0F82F2000000     JB L648

;; 11 byte / condition


(defun interpret-iteration-over-leaf (op-id-sym state-sym sg body)
  ;; (log:warn "falling back to the interpretation based successor generation")
  `(labels ((rec (node)
              (ematch node
                ((type list)
                 (dolist (,op-id-sym node)
                   ,@body))
                ((sg-node variable then else either)
                 (if (= 1 (aref ,state-sym variable))
                     (rec then)
                     (rec else))
                 (rec either)))))
     (rec ,sg)))
