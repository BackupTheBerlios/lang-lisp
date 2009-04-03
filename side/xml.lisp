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

(defpackage #:xml-out
  (:use #:common-lisp #:generic)
  (:export produce-xml-fun produce-xml-string
	   produce-xml-stream produce-xml-file
	   attr no-close)
  (:documentation "Makes xml data from nested tree data."))

(in-package #:xml-out)

(defun produce-xml-fun (code produce-fun &key (level 0)
			   (attribute 'attr) (no-close 'no-close))
  "Produce xml data based on nested tree data.
First element of a list is the <name>
Attribute(default 'attr) is the tag it 
will look for in lists in the second element to add attributes."
  (flet ((produce (code) 
	   (produce-xml-fun code produce-fun :level (+ level 1)
			       :attribute attribute :no-close no-close))
	 (add-str (str)
	   "Adds a string, keeping tabs on line length."
	   (funcall produce-fun str level)))
    (cond
      ((not (listp code)) ;Just lone objects.
       (add-str (if (stringp code) (format nil "\"~D\"" code) code)))
      (t
       (let ((name (format nil "~D" (car code))))
	 (cond
	   ((let ((second (second code)))
	      (when (and second (listp second)) ;There are attributes
		(eql (car second) attribute)))
	    (add-str (format nil "<~D" name))
	    (dolist (c (cdr (second code))) ;Write them.
	      (add-str (format nil "~D=" (car c)))
	      (produce (cadr c)))
	    (setf- cdr code)
	  ;See if it closes at this point.
	    (add-str (cond
		       ((eql (aref name 0) #\?)
			"?>")
		       ((eql (cadr code) no-close)
			"\>")
		       (t
			">"))))
	   (t
	    (add-str (format nil (if (eql (cadr code) no-close)
				     "<~D \>" "<~D>") name))))
	 (unless (or (case (aref name 0) ((#\? #\!) t))
		     (eql (cadr code) no-close)) ;Write the 'body'
	   (dolist (c (cdr code)) (produce c))
	   (add-str (format nil "</~D>"  name))))))))

(defun produce-xml-stream (code stream &key (level 0)
			   (tab-size 2) line-length
			   (attribute 'attr) (no-close 'no-close))
  "Does what produce-xml-fun does, fills in the produce-fun to make it \
write to stream."
  (let ((cur-len 0) (out-str ""))
    (produce-xml-fun code
       (lambda (code level)
	 (when-with got
	     (cond
	       ((eql code #\Newline) ;These can make newlines manually.
		(setf cur-len 0)
		(format stream "~%~D" (format nil "~~~DT" level)))
	       (t
		(let ((str (format nil " ~D" code))) ;Keep count of line length.
		  (setf- + cur-len (length str))
		 (cond ;Too-much => next line.
		   ((when line-length (> cur-len line-length))
		    (setf cur-len (+ (* tab-level tab-size) (length str)))
		    (format stream "~%~D~D" (format nil "~~~DT" level) str))
		   (t ;No line checking or enough, just write.
		    (format stream str))))))
	   (when (stringp got)
	     (setf out-str (concatenate 'string out-str got)))))
       :attribute attribute :no-close no-close)
    out-str))

(defun produce-xml-file (code file-name &key (level 0)
           (if-does-not-exist :create) (if-exists :supercede)
	   (tab-size 2) line-length
	   (attribute 'attr) (no-close 'no-close))
  "Just a wrapper of produce-xml-stream, making the stream for you."
  (with-open-file (stream file-name :direction :output
	   :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (produce-xml-file code stream :level level
       :attribute attribute :no-close no-close
       :tab-size tab-size :line-length line-length)))

(defun produce-xml-string (code &key (level 0)
			   (tab-size 2) line-length
			   (attribute 'attr) (no-close 'no-close))
  (produce-xml-file code nil :level level
		    :attribute attribute :no-close no-close
		    :tab-size tab-size :line-length line-length))
