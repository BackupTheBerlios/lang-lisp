
(in-package #:lang)

(defclass conv-state ()
  ((conv-fun :initarg :fun)
   (conv-value :initarg :value)
   (conv-macs :initarg :macs)))

(defun make-conv-state (fun value macs)
  (make-instance 'conv-state :fun fun :value value :macs macs))

(defun conv-code (code conv-state)
  "Converts lang resolved code into something else.
It is not flexible enough to do C on purpose, the plan is to have\
 transformations on resolved code instead, changing its properties so that\
 this can convert it."
  (unless (listp code)
    (setf- list code))
  (with-slots (conv-fun conv-value conv-macs) conv-state
    (case (type-of (car code))
      (fun
       (funcall conv-fun code conv-state))
      (value
       (funcall conv-value code conv-state))
      (out
       (if-with mac-fun (gethash (slot-value (car code) 'name) conv-macs)
	 (funcall mac-fun code conv-state)
	 (error "Couldn't find conversion for a macro output."))))))

(defmacro make-conv (hash name (&rest args) &body body)
  "Makes making a conv more convenient.
 Makes variables self, code and conv-state, and conv in flet."
  `(setf (gethash ,name *conv-lisp-macs*)
	 (lambda (code conv-state)
	   (flet ((conv (c)
		    (conv-code c conv-state)))
	     (argumentize-list (self ,@args) code
	       ,@body)))))

