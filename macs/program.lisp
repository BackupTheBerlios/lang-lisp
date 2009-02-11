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

(rawmac-add |program| () () (name (&rest type-args) (&rest args) &rest body)
  "A program is something you can write variables, defvar, functions(defun)\
 and structures in. TOP is automatically the first program created at load
time. The TOP program has variables as global variables and does not have
a struct; to-c.lisp makes it the main(argc,argv) function.
If it is not top, it has a namespace(named name), a main function and
 a structure named (format nil prog-~D name), all the defvars go into\
 the structure and all the functions take the program structure as 
argument."
  (with-fun-resolve
  (let (struct main-body
	(struct-name (intern (format nil "prog_~D" name))))
    (dolist (b body)
      (cond
	((not (listp b))
	 (push (resolve b type-of) main-body))
	(t
	 (case (car b)
	   ((defun |defun|)
	 ;Add the structure to to function as argument and enable its slots.
	    (let ((rest (cdr b)) keys)
	      (do () ((symbolp b) nil)
		(push (car b) keys)
		(setf- cdr rest))
	      (resolve `(defun ,(cadr b) ,@keys
			  ((|prog| (,struct-name ,@type-args)),@(car rest))
			  (with-slots '|all| |prog|
			    ,@(cdr rest)))
		       '())))
	   ((will-defun |will-defun|)
	    (argumentize-list (name out-type &rest arg-types) (cdr b)
	      (resolve `(will-defun ,name ,out-type
				    (,struct-name ,@type-args)
				    ,@arg-types))))
	   ((defvar |defvar|)
        ;Add variables to the structure.
	    (argumentize-list (var-name set-to) (cdr b)
	      (let ((res (resolve `(|ref| ,set-to) type-of)))
		(push (list var-name (out-type res)) type-of)
		(push (list var-name (cadr(out-type res))) struct)
		(push (resolve `(set ,var-name ,res) type-of) main-body))))
	   (t
	    (push (resolve b type-of) main-body))))))
