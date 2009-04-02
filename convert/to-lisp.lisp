
(in-package #:lang)

(defclass conv-state ()
  ((fun :initarg :fun)
   (value :initarg :value)
   (macs :initarg :macs)))

(defun make-conv-state (fun value macs)
  (make-instance 'conv-state :fun fun :value value :macs macs))

(defun conv (code conv-state)
  "Converts lang resolved code into something else.
It is not flexible enough to do C on purpose, the plan is to have\
 transformations on resolved code instead, changing its properties so that\
 this can convert it."
  (unless (listp code)
    (setf- list code))
  (with-slots (fun value macs) conv-state
    (case (type-of (car code))
      (fun
       (funcall fun code conv))
      (value
       (funcall value code conv))
      (out
       (if-with mac-fun (gethash (slot-value (car code) 'name) macs)
	 (funcall mac-fun code conv-state)
	 (error "Couldn't find conversion for a macro output."))))))

(defvar *conv-lisp-macs* (make-hash-table))

(defun lisp-conv-fun (code conv-state)
  "Function useage conversion to lisp."
  `(,(get-name fun :lisp)
     ,@(iter (for c in (cdr code))
	     (collect (conv-code c conv-state)))))

(defun lisp-conv-value (code conv-state)
  "Value useage conversion to lisp."
  (declare (ignored conv-state))
  (from (car code)))

(defun make-lisp-conv-state ()
  (make-conv-state #'lisp-conv-fun #'lisp-conv-value *conv-lisp-macs*))
