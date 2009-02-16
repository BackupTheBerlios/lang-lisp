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

;TODO better error reporting.
;TODO Seems a little cryptic, add some explaning comments.
(defun arguments-to-position (args list)
  "Extracts arguments from a list. (pass unwraps them)"
  (let ((i 0) (out nil) (key -1))
  (dolist (e args)
    (cond
      ((listp e)
       (case key
	 (-1 (setf- append out (arguments-to-position e `(nth ,i ,list))))
	 (-2 (error "Found list after &optional (macro side)"))
	 (-3 (error "Found list after &rest (macro side)"))
	 (t  (push `(,(car e) (if-use (getf (nthcdr ,key ,list) ',(car e))
				      ,(cadr e)))
		   out))))
      (t
       (case e
	 (cl:&optional
	  (case key
	    (-1 (setf key -2))
	    (-2 (error "&optional twice."))
	    (-3 (error "&optional after &rest"))
	    (t  (error "&optional after an &key."))))
	 (cl:&key
	  (case key
	    (-1 (setf key i))
	    (-2 (error "&key after &optional"))
	    (-3 (error "&key after &rest"))
	    (t  (error "twice &key."))))
	 (cl:&rest
	  (case key
	    (-1 (push `(,(nth (+ i 1) args) (nthcdr ,i ,list))
		      out)
		(setf key -3))
	    (-2 (error "&rest after &optional"))
	    (-3 (error "&rest twice"))
	    (t  (error "&rest after &key."))))
	 (t
	  (case key
	    ((-1 -2)
	     (push `(,e (nth ,i ,list)) out))
	    (-3
	     nil)
	    (t
	     (push `(,e (getf (nthcdr ,key ,list) ',e)) out)))))))
    (setf- + i 1))
  out))

(defmacro argumentize-list ((&rest arguments) list &body body)
"Allows you to make a list act like a bunch of arguments in the style of a
macro."
  `(let ,(arguments-to-position arguments list)
     ,@body))

