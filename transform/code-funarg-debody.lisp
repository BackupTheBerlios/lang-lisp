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
	(make-let (cadar prepend)
		  (list (actually-prepend res (cdr prepend)))))
     ;TODO WTF: replace make-let with list.
       (t
	(error "Prepending keyword not recognized."))))
    (t ;It is like a progn, put in-series.
     (make-progn
      (iter
	(for p on prepend)
	(cond
	  ((keywordp (caar p)) ;Keywords need sub-let (or similar).
	   (collect (actually-prepend res p))
	   (finish))
	  ((null (cdr p)) ;End of the line, end with result.
	   (collect (car p))
	   (collect res))
	  (t ;Not end of the line, just prepend.
	   (collect (car p)))))))))

;;TODO improve, maybe some variable tracker-along-function?
(defun code-funarg-debody
    (code &key (body-level t)
     rename-var
     (gen-name (lambda (name) (format nil "~D_~D" name (gensym)))))
  "Transforms code, taking out all the progns and lets in argument.
This is needed for converting to C, which doesn't allow for bodies or\
 variable creation in arguments.
TODO figure out flets."
;  (print (list code :bl body-level))
  (let (prepend) ;Stuff, body and variables that needs to be prepended.
    (flet ((c-return (code)
	     "Returns code with any added things to be prepended."
	     (if body-level
	       (actually-prepend code prepend)
	       (values code prepend)))
	   (do-code (code &key (bl nil))
	     "Does code-funarg-debody for given code, and adds things to be\
 prepended."
	     (multiple-value-bind (ret more-prepend)
		 (code-funarg-debody code :body-level bl
				     :rename-var rename-var)
	       (setf- append prepend more-prepend)
	       ret)))
     (flet ((do-body (code &key (bl nil))
	     (iter (for c in code)
		   (collect (do-code c :bl bl))))
	    (rename-var (name)
	      "Renames a variable to avoid name clashes."
	      (let ((new-name (funcall gen-name name)))
		(setf rename-var (append (list name new-name) rename-var))
		new-name))
	    (get-var (var)
	      "Gets a variable, being name itself when not renamed."
	      (let ((new-name (getf rename-var (from var))))
		(if new-name
		    (make-instance 'value
				   :type (out-type var) :from new-name)
		    var))))
       (c-return
	(case (type-of code)
	  (applied-fun
	   (with-slots (fun args out-type) code
	     (make-instance 'applied-fun
	       :fun fun :args (do-body args) :out-type out-type)))
	  (-progn
	   (with-slots (body) code
	     (cond
	       (body-level
		(make-progn (do-body body :bl t)))
	       (t ;Add code to prepend. 
		(setf- append prepend (do-body (butlast body)))
		(do-code (car(last body)))))))
	  (-let ;Let, move variables before it.
	   (with-slots (vars body) code
	     (let ((done-vars
		    (iter (for v in vars)
			  (collect (list (car v) (do-code (cadr v)))))))
	       (cond
		 (body-level
		  (make-let done-vars (do-body body :bl t)))
		 (t ;Add code to prepend, marked as a let.
		  (let ((let-var
			 (iter
			   (for c in done-vars)
			   (for v in vars)
			   (collect `(,(rename-var (car v)) ,(cadr c))))))
		    (setf prepend `(,@prepend (:arg-let ,let-var)))
		    (do-code (make-progn body))))))))
	  (value
	   (if (symbolp (from code)) (get-var code) code))
	  (t
	   (error "Did not recognize end-structure. ~D" code))))))))
