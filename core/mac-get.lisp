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
(in-package #:lang)

;Macros are just functions that operate on code.
(defclass mac (documented)
  ((name :initarg :name :initform :no-name)
   (fun  :initarg :fun :initform (lambda (code)(cdr code))
	 :accessor fun)))

(defun mac-get (local name)
  "Gets a macro."
  (symget local *macs* name))

(defun (setf mac-get) (to local name)
  "Sets a macro."
  (setf (symget local *macs* name) to))

(defmacro mac-add (name (&key (local '*local*)
			      (code (gensym)) (*local (gensym)))
		   (&rest arguments)
		   &body body)
  "Creates basic macros."
  (let ((doc-str (when (stringp (car body))
		   (car body))))
    `(let ((,*local ,local))
       (setf (mac-get ,*local ',name)
	     (make-instance 'mac :name ',name
	         :doc-str ,doc-str
		 :fun (lambda (,code ,*local)
			(argumentize-list (,@arguments) (cdr ,code)
			  ,@body)))))))
