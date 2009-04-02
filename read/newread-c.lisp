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

(defpackage #:read-c
  (:use #:common-lisp #:iterate #:str-read)
  (:export read-c))

(in-package #:read-c)

(defun non-symbol (ch)
  "Characters that are not part of symbols."
  (case ch ((#\Newline #\Space #\Tab #\) #\( #\| #\;
	     #\* #\, #\=) t)))

(defvar *comments* `(("/*" ,#'clause-comment)
		     ("//" ,#'clause-line-comment)))

(defun read-c-macrolike (reader)
  "Reads C macro stuff. TODO currently only #define"
  (iter
    (reader-skip reader)
    (cond-reader-next-str= reader
      (("/*" :skip)      (read-skip-to reader "*/"))
      (("//" :skip)      (read-skip-to reader #\Newline))
      (("#")             (error "Two # in sequence."))
      ((("DEFINE" "define") :skip)
       (collect "define")
       (reader-skip reader)
       (collect (read-token reader :skip))
       (when (reader-next-str= reader "(")
	 (error "No support for C functional macros."))
       (reader-skip reader)
       (collect (read-token reader :skip))
       (finish)))
    (when (reader-at-end reader)
      (finish))))

(defun read-c-struct (reader)
  "Reads a structure. TODO"
  (reader-skip reader)
  (let*((name (read-token reader :skip))
	in-body body-elements)
    (cond-reader-next-str= reader
      (("{" :skip)
       ))))

(defun read-c-typedef (reader)
  (reader-skip reader)
  (if-use
   (cond-reader-next-str= reader
     (("struct" :skip)
      `("typedef" ,(read-c-struct reader) ,(read-token reader :skip))))
   `("typedef" ,(read-token reader :skip) ,(read-token reader :skip))))

(defun read-c-value (reader results toklist)
  ) ;TODO

(defun read-c-function (reader results toklist)
  ) ;TODO

(defun read-c (reader results)
  "Reads C header files, returns results via callback results."
  (iter
    (reader-skip reader) ;Skip whitespace.
    (cond-reader-next-str= reader
      (("/*" :skip)
       (read-skip-to reader "*/"))
      (("//" :skip)
       (read-skip-to reader #\Newline))
      (("#" :skip)
       (funcall results (read-c-macrolike reader results)))
      (("struct" :skip)
       (funcall results (read-c-struct reader)))
      (("typedef" :skip)
       (funcall results (read-c-typedef reader)))
      (("" ;Functions/values, no indication ahead, so collect tokens.
	(let ((toklist (list)))
	  (flet ((read-tok ()
		   (push (reader-token reader :skip) toklist)))
	    (iter
	      (cond-reader-next-str=
	       (("/*" :skip) (read-skip-to reader "*/"))
	       (("//" :skip) (read-skip-to reader #\Newline))
	       (("=" :skip)  (read-c-value reader results toklist))
	       (("(" :skip)  (read-c-function reader results toklist))
	       ((";" :skip)  (return))) ;Don't know what just was read.
	      (when (reader-at-end reader)
		(return))))))))
    (when (reader-at-end reader)
      (return))))

(defun read-typedef (results)
  "Reads a structure. TODO"
  (lambda (getstr str)
    (let (type type-name)
      (reader str getstr #'is-whitespace
	`(,@*comments*
	  ("struct"
	   ,(lambda (getstr str)
	       (when type (error "Already have type."))
	       (multiple-value-bind (new-str ret struct-def)
		   (funcall (read-struct results) getstr str)
		 (setf type struct-def)
		 new-str)))
	  (""
	   ,(lambda (getstr str)
	      (cond
		(type (setf type-name (get-token str #'non-symbol))
		      (funcall results `(typedef ,type type-name))
		      (values (subseq str (length type-name)) t)) ;Done.
		(t    (setf type (get-token str #'non-symbol))
		      (subseq str (length type)))))))))))

(defun continue-value (got results)
  "Continues value-reading. TODO only works if there is a =, doesn't work 
for multiple values at a time."
  (lambda (getstr str)
    (values
     (reader str getstr #'is-whitespace
       `(,@*comments*
	 (";" ,(lambda (getstr str)
		 (values str t)))
	 ("," ,(lambda (getstr str) str))
	 (""  ,(lambda (getstr str)
		 (let ((value (get-token str #'non-symbol)))
		   (funcall results `(value ,(car got)
					    (,@(reverse(cdr got)))
					    ,value))
		   (subseq* str (length value))))))
       :limit 100)
     :done)))

(defun continue-function (got results)
  "Continues function-reading.(Currently only headers!)"
  (lambda (getstr str)
    (values
     (let (one-arg args)
       (reader str getstr #'is-whitespace
	 `(,@*comments*
	   ("," ,(lambda (getstr str)
		   (push one-arg args)
		   (setf one-arg nil)
		   str))
	   (")" ,(lambda (getstr str)
		   (push one-arg args)
		   (funcall results `(function ,(car got)
					       (,@(reverse(cdr got)))
					       (,@(reverse args))))
		   (values (reader str getstr #'non-symbol
			     `(,@*comments*
			       (";" (lambda (str getstr) (values str t))))
			     :limit 100)
			     t)))
	   (""  ,(lambda (getstr str)
		   (push (get-token str #'non-symbol) one-arg)
		   (subseq* str (length (car one-arg))))))
	 :limit 100))
     :done)))

