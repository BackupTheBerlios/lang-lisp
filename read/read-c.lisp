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
  (:use #:common-lisp #:generic #:read)
  (:export read-c))

(in-package #:read-c)

(defun non-symbol (ch)
  "Characters that are not part of symbols."
  (case ch ((#\Newline #\Space #\Tab #\) #\( #\| #\;
	     #\* #\, #\=) t)))

(defun subseq* (str start &optional end)
  "Subseq without bounds errors."
  (if (> (length str) start)
      (subseq str start (if end (min end (length str)) (length str)))
      ""))

(defun in-the-list (list)
  (lambda (ch) (in-list list ch)))

(defvar *comments* `(("/*" ,#'clause-comment)
		     ("//" ,#'clause-line-comment)))

(defun read-c-stuff (getstr &optional (str "") results)
  "Reads C macro stuff. TODO currently only #define"
  (reader str getstr #'is-whitespace
    `(,@*comments*
      ("#" ,(lambda (getstr str) (error "Two # in sequence.")))
      ("define"
       ,(lambda (getstr str)
	  (let ((sym "") (val ""))
	    (setf- reader str getstr #'is-whitespace
		   `(,@*comments*
		     ("" ,(lambda (getstr str)
			    (setf sym (get-token str #'non-symbol))
			    (values (subseq* str (length sym)) t))))
		   :limit 100)
	    (setf- reader str getstr #'is-whitespace
		   `(,@*comments*
		     ("(" ,(lambda (getstr str)
			     (warn "No support for C functional macros.")))
		     (""  ,(lambda (getstr str)
			     (setf val (get-token str #'non-symbol))
			     (values (subseq* str (length val)) t))))
		   :limit 100)
	    (funcall results `(define ,sym ,val))
	    (values str t))))
      ("\\" ,(lambda (getstr str)
	       (if (= (length str) 0)
	         (funcall getstr) str)))
      (""   ,(lambda (getstr str)
	       (error "# stuff error?"))))
    :limit 100))

(defun read-c (getstr &optional (str "") results)
  "Reads C header files, returns results via callback."
  (reader str getstr #'is-whitespace
    `(,@*comments*
      ("#" ,(lambda (getstr str) ;Macro-like stuff.
	      (read-c-stuff getstr str results)))
      ("struct"  ,(read-struct results)) ;Structures without typedef.
      ("typedef" ,(read-typedef results)) ;Type definitions.
      (""  ,(lambda (getstr str) ;Functions and values.
	      (let ((toklist (list)))
		(flet ((read-tok (getstr str)
			 (push (get-token str #'non-symbol) toklist)
			 (values (subseq* str (length (car toklist)))
				 t)))
		  (do ()
		      ((multiple-value-bind (new-str stop)
			   (reader str getstr #'is-whitespace
			     `(,@*comments*
			       ("*" ,(lambda (str getstr)
				       (push "*" toklist)
				       str))
			       ("=" ,(continue-value toklist results))
			       ("(" ,(continue-function toklist results))
			       (""  ,#'read-tok))
			     :limit 100)
			 (setf str new-str)
			 (case stop
			   (:done t)
			   (t     (= (length str) 0))))
		       str)))))))
    :limit 100))

(defun read-struct (results)
  "Reads a structure. TODO"
  (lambda (getstr str)
    (let (name in-body body-elements)
      (values
       (reader str getstr #'is-whitespace
	 `(,@*comments*
	   ("{",(lambda (getstr str)
		  (setf in-body t)))
	   ("}",(lambda (getstr str)
		  (unless in-body (error "Should be in body for }"))
		  (funcall results `(struct ,name ,body-elements))
		  (values str t)))
	   ("" ,(lambda (getstr str)
		  (cond
		    (start-body 
		     (let (type vars)
		       (reader str getstr #'is-whitespace
			 `(,@*comments*
			   ("}",(lambda (getstr str)
				  (error "Early }, needed ; first")))
			   (";",(lambda (getstr str)
				  (unless vars ;No variables started yet.
				    ;That one was var.
				    (setf vars (list(car type))))
				  (values str t))) ;This type done.
			   (",",(lambda (getstr str)
				  (setf vars (list(car type))) ;Start vars.
				  str))
			   (""
			    ,(lambda (getstr str)
			       (cond
				 (vars
				  (push (get-token str #'non-symbol) vars))
				 (t
				  (push (get-token str #'non-symbol) type)
				  (subseq str (length(car type)))))))))
		      ;Register the read things.
		       (push `(,(reverse type) ,(reverse vars))
			     body-elements)))
		    ((not name) ;Get name of struct.
		     (setf name (get-token str #'non-symbol))
		     (subseq* str (length name)))
		    (t
		     (error "Structs can only have one name.")))))))
       nil ;Read by reader.
       `(struct ,name ,body-elements))))) ;The goods again.

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
