;;
;;  Copyright (C) 2009-02-07 Jasper den Ouden.
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

(defpackage #:simple-macexpand
  (:use #:common-lisp #:generic #:namespace))

(in-package #:simple-macexpand)

(defclass state (namespace-state)
  ((const :initform (make-hash-table))
   (macs :initform (make-hash-table))))

(defvar *state* (make-instance 'state))

(defun add-mac-fun (name fun &key (state *state*))
"Add a macro function. (The macro add-mac might be more convenient to use.)"
  (setf (get-symbol name (slot-value state 'macs) state) fun))

(defmacro add-mac (name (&rest arguments)
	   (&key (state '*state*) (code-var (gensym)) (state-var (gensym)))
	   &body body)
  "Adds a macro. Add your arguments in args, set code-var and state-var if 
you want to use them manually."
  (print`(let ((,state-var ,state))
     (add-mac-fun ,name
       (lambda (,code-var ,state-var)
	 (generic:argumentize-list (,@arguments) (cdr ,code-var)
	   ,@body))
       :state ,state))))

(defun resolve (code &key (state *state*))
  "Resolves all the macros."
  (with-slots (const macs) state
    (cond
      ((symbolp code) ;Check symbols for constant values.
       (if-use (get-symbol code const state) code))
      ((not (listp code)) ;Otherwise return as is.
       code)
      (t
       ;Macroexpand until there is no more.
       (do ((mac-fun (get-symbol (car code) macs state)
		     (get-symbol (car code) macs state)))
	   ((null mac-fun) t)
	 (multiple-value-bind (new-code message)
	     (funcall mac-fun code state)
	   (setf code new-code)
	   (case message
	     (:stop (return)))))
       ;Do lower levels and return it.
       (cons (car code) (loop for c in (cdr code)
			  collect (resolve c :state state)))))))
