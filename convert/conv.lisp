;;
;;  Copyright (C) 2009-04-03 Jasper den Ouden.
;;
;;  This file is part of Lang(working title).
;;
;;  Lang is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Lang is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with Lang.  If not, see <http://www.gnu.org/licenses/>.
;;

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
	 (error (format nil 
			"Couldn't find conversion for a macro output ~D."
			(slot-value (car code) 'name))))))))

(defmacro make-conv (hash name (&rest args) &body body)
  "Makes making a conv more convenient.
 Makes variables self, code and conv-state, and conv in flet."
  `(setf (gethash ,name ,hash)
	 (lambda (code conv-state)
	   (flet ((conv (c &optional (state conv-state))
		    (conv-code c state)))
	     (argumentize-list (self ,@args) code
	       ,@body)))))

