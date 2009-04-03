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

;;TODO noticed a nil sneaking in with lets.

(defun actually-prepend (res prepend)
  "Internal function for code-funarg-debody, which puts variable creation\
 and bodies in function arguments befor the result."
  (cond
    ((null prepend) ;At end, just result.
     res)
    ((keywordp (caar prepend))
     (case (caar prepend)
       (:arg-let  ;It is a let, make it and put stuff subordinate.
	`(,(make-instance 'out :name 'let)
	  ,(second(car prepend))
	  ,(actually-prepend res (cdr prepend))))
       (t
	(error "Prepending keyword not recognized."))))
    (t ;It is like a progn, put in-series.
     (cons (make-instance 'out :name 'progn)
	   (iter
	     (for p on prepend)
	     (cond
	       ((keywordp (car p)) ;Keywords need sub-let (or similar).
		(collect (actually-prepend res p))
		(finish))
	       ((null (cdr p)) ;End of the line, end with result.
		(collect (car p))
		(collect res))
	       (t ;Not end of the line, just prepend.
		(collect (car p)))))))))

(defun code-funarg-debody (code &key (body-level t)
			   rename-var (gen-name #'gensym))
  "Transforms code, taking out all the progns and lets in argument.
This is needed for converting to C, which doesn't allow for bodies or\
 variable creation in arguments.
TODO figure out flets."
  (unless (listp code)
;    (warn (format nil "Process-code accidentally ate a non-list. ~D" code))
    (setf- list code))
  (let (prepend) ;Stuff, body and variables that needs to be prepended.
    (flet ((c-return (code)
	     "Returns code with any added things to be prepended."
	     (values code prepend))
	   (do-code (code &key (bl nil))
	     "Does code-funarg-debody for given code, and adds things to be\
 prepended."
	     (multiple-value-bind (ret more-prepend)
		 (code-funarg-debody code :body-level bl
				     :rename-var rename-var)
	       (setf- append prepend more-prepend)
	       ret))
	   (rename-var (name)
	     "Renames a variable to avoid name clashes."
	     (let ((new-name (funcall gen-name)))
	       (setf rename-var (append (list name new-name) rename-var))
	       new-name))
	   (get-var (var)
	     "Gets a variable, being name itself when not renamed."
	     (let ((new-name (getf rename-var (from var))))
	       (if new-name
		 (list (make-instance 'value
				      :type (out-type var) :from new-name))
		 (list var)))))
      (case (type-of (car code))
	(fun ;Handle arguments of function.
	 (cond
	  ;Actually prepends the stuff that the rest in effect asks for.
	   (body-level
	    (let ((res (do-code code)))
	      (actually-prepend res prepend)))
	   (t
	    (c-return (cons (car code)
			    (iter (for c in (cdr code))
				  (collect (do-code c))))))))
	(value ;Nothing to handle. (couldnt have cought anything in prepend)
	 (if (symbolp (from(car code))) (get-var (car code)) code))
	(out ;End-macro results.
	 (case (slot-value (car code) 'name)
	   (progn
	     (cond
	       (body-level
		(cons (car code)
		      (iter (for c in (cdr code))
			    (collect (do-code c :bl t)))))
	       (t ;Add code to prepend. 
		(setf- append prepend
		       (iter (for c in (butlast (cdr code)))
			     (collect (do-code c :bl t))))
		(c-return (do-code (car(last code)))))))
	   (let ;Let, move variables before it.
	     (cond
	       (body-level
		(c-return (list (first code) (second code)
				(do-code (third code) :bl t))))
	       (t ;Add code to prepend, marked as a let.
		(let*((done-code (iter (for v in (second code))
				       (collect (do-code (cadr v)))))
		      (let-var
		       (iter (for c in done-code)
			     (for v in (second code))
			     (collect `(,(rename-var (car v)) ,c)))))
		  (setf prepend `(,@prepend (:arg-let ,let-var))))
		(c-return (do-code (third code))))))
	   (t
	    code)))))))
