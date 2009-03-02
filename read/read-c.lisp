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
  (:use #:common-lisp #:generic #:reader)
  (:export ))

(in-package #:read-c)

(defconst *comments* (list ("//" #'read-line-comment) ("/*" #'read-comment)))

(defun read-c-stuff (getstr &optional (str "") results)
  "Reads C macro stuff. TODO doesn't do anything like if, etc."
  (reader str getstr 
    `(,@*comments*
      ("#" ,(lambda (getstr str) (error "Two # in sequence.")))
      ("define"
       ,(lambda (str getstr)
	  (let ((sym "") (val ""))
	    (setf- reader str getstr
		   `(,@*comments*
		     ,("" (lambda (getstr str)
			    (setf sym (get-token str))
			    (values (subseq* str (length sym)) t)))))
	    (setf- reader str getstr
		   `(,@*comments*
		     ,("(" (lambda (getstr str)
			     (warn "No support for C functional macros.")))
		     ,(""  (lambda (getstr str)
			     (setf val (get-token str))
			     (values (subseq* str (length val)) t)))))
	    (funcall results `(define ,sym ,val))
	    (values str t))))
      ("\\" (lambda (str getstr) (if (= (length str) 0)
				   (funcall getstr) str)))
      ("" (lambda (str getstr)
	    (values str :skip-str))))))

(defun read-c (getstr &optional (str "") results)
  "Reads C header files, returns results via callback."
  (reader str getstr
    `(,@*comments*
      ("#" ,(lambda (getstr str) (read-c-stuff getstr str results)))
      (""  ,(lambda (getstr str)
	      (let ((toklist (list (get-token str))))
		(flet ((read-tok (getstr str)
			 (push (get-token str) toklist)
			 (values (subseq* str (length (car toklist)))
				 t)))
		  (do ()
		      ((multiple-value-bind (new-str stop)
			   (reader str getstr
			     `(,@*comments*
			       ("*" ,(lambda (str getstr)
				       (push "*" toklist)
				       str))
			       ("=" ,(continue-value toklist results))
			       ("(" ,(continue-function toklist results))
			       (""  ,#'read-tok)))
			 (case stop
			   (:done (return new-str)))
			 (setf str new-str)))))))))))

(defun continue-value (got results)
  "Continues value-reading."
  (lambda (getstr str)
    (values
     (reader str getstr
       `(,@*comments*
	 (";" ,(lambda (getstr str)
		 (values str t)))
	 ("," ,(lambda (getstr str) str))
	 (""  ,(lambda (getstr str)
		 (let ((value (get-token str)))
		   (funcall results `(value ,(car got)
					    (,@(reverse(cdr got)))
					    ,value)))
		(subseq* str (length value))))))
     :stop)))

(defun continue-function (got results)
  "Continues function-reading."
  (lambda (getstr str)
    (values
     (let (one-arg args)
       (reader str getstr
	 `(,@*comments*
	   ("," ,(lambda (getstr str)
		   (push one-arg args)
		   (setf one-arg nil)
		   str))
	   (")" ,(lambda (getstr str)
		   (funcall results `(function ,(car got)
					       (,@(reverse(cdr got)))
					       (,@args)))
		   (values (reader str getstr
			     `(,@*comments*
			       (";" (lambda (str getstr) (values str t)))))
			     t)))
	   (""  ,(lambda (getstr str)
		   (push (get-token str) one-arg)
		   (subseq* str (length (car one-arg))))))))
     t)))

(defun getstr-stream (stream)
  (lambda () (read-line stream)))
