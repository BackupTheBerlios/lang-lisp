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

(defclass fun-variant ()
  ((arg-types :documentation "Types of the arguments."
	      :initarg :arg-types :type list)
   (res :documentation "Resolved code."
	:initarg :res)
   (names :documentation "Names the function has in different output\
 languages."
    :initarg :names :initform nil :type list)))

(defclass fun (documented)
  ((name :initform nil :type (or null symbol) :initarg :name)
   
   (out-type-fn :documentation "Possibly a function, if returns non-nil,\
 overrides out-type."
    :initarg :out-type-fn :initform nil :type (or function null))
   
   (args-code :documentation "Arguments of the function."
    :initarg :args-code :initform nil)
   (body-code :documentation "Body of the code, as it was entered."
    :initarg :body-code :initform nil)
   
   (flags :documentation "Flags the code has."
	  :initarg :flags :initform nil)
   
   (variants :documentation "Already encounted variants."
	     :initform nil :initarg :variants :type list)
   
   (names
    :documentation "Names the function has in different output languages."
    :initarg :names :initform nil :type list))
  (:documentation "Holds the information of functions in Lang."))

(defun function-p (obj)
  (eql (type-of obj) 'fun))

(defmethod out-type ((fun fun))
  (with-slots (variants) fun
    (case (length variants)
      (0 '(|unused-fun|))
      (1 `(|function| ,@(slot-value (car variants) 'arg-types)))
      (t `(|or|
	   ,@(iter (for v in variants)
		   (collect `(|function| ,@(slot-value v 'arg-types)))))))))

(defmethod out-type ((list list))
  (out-type (car list)))

(defun fun-get (local name)
  "Gets the function."
  (symget local *funs* name))

(defun (setf fun-get) (to local name)
  "Adds a function.(Does not replace old function unless exact!)"
  (setf (symget local *funs* name) to))

(defmacro fun-add (local name &rest rest)
  "Makes a function with (setf fun-get), shaves off make-instance."
  (with-gensyms (the-name *local)
    `(let*((,*local ,local)
	   (,the-name ,name))
       (setf (fun-get ,*local ,the-name)
	     (make-instance 'fun :name ,the-name ,@rest)))))

(defmacro extend-if-nil (fun (&rest args) &body new)
  "The extension must have the same arguments as the new one.\
 Doesn't do any &rest &optional etc. currently."
  (with-gensyms (old-fun)
    `(let ((,old-fun ,fun))
       (lambda (,@args)
	 (if-use (funcall ,old-fun ,@args) (progn ,@new))))))
