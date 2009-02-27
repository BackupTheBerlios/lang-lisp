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

(defun function-creator (out-name code type-of &key state)
  "Creates functions. (Used in lang functions flet, defun and lambda.)"
  (with-fun-resolve
  (let ((name (car code)) (rest (cdr code)) only-record
	got-flags)
  ;See if there are any relevant markers.
    (loop while (symbolp (car rest))
       do (case (car rest)
	    (|:inline|
	     (push :inline got-flags))
           ;Only makes a record of it, intended to be used along with 
           ; :specify-as-used (If true, only more specific versions usable.
	    (|:only-record|
	     (setf only-record t)
	     (push :only-record got-flags))
           ;Creates the more specific functions if they are used.
	    (|:specify-as-used| (push :specify-as-used got-flags)))
       do (setf- cdr rest))
    (argumentize-list ((&rest args) &rest body) rest
      (setf args (loop for a in args collect (if (listp a) a `(,a (any)))))
  ;Explicitly specified out-type.
      (let*((doc-str (when (stringp (car body))
		       (let ((tmp (car body)))
			 (setf- cdr body) tmp)))
	    (res (unless only-record
		   (resolve `(progn-raw ,@body) (append type-of args))))
	    (arg-types
	     (loop for a in args
		collect (if (listp a) (cadr a) `(,a (any)))))
           ;Add function.
	    (fun (fun-add name arg-types (:state state)
			  :flags (append got-flags
				  (case out-name 
				    (lambda '(:lambda))
				    (flet   '(:flet))))
			  :doc-str (if-use doc-str "")
			  :full-code `(defun ,@code) :code res
			  :out-type (if only-record '(error)
				      (out-type(car res))))))
      ;Return the function. (Although not really meant as used functionally.)
      ;Lambda would be for that.
	`(,(make-instance 'out :name out-name
			  :type `(function ,(if only-record '(error)
					      (out-type(car res)))
					   ,@arg-types))
	   ,fun))))))

(rawmac-add defun (:code code) () (name &rest stuff)
  (cond
    ((listp name)
     (unless (eql (car name) '|set|)
       (error "Invalid form of defun."))
     (values `(defun ,(intern (format nil "set_~D" (cadr name))) ,@stuff)
	     :again))
    (t
     (function-creator 'defun (cdr code) type-of :state state))))

(rawmac-add set () () (what to)
  (with-fun-resolve
    (let ((to   (resolve to type-of)))
      (cond
	((symbolp what)
	 (let ((what (value-resolve what type-of :state state)))
	   (unless (type-coarser (out-type what) (out-type to) :state state)
	     (error "Trying to set a variable to something it is not \
general enough for."))
	   `(,(make-instance 'out :name 'set) ,what ,to)))
	((listp what)
	 (values `(,(intern (format nil "set_~D" (car what)))
		    ,to ,(cdr what))
		 :again))
	(t
	 (error "Set Only works with set_ functions and symbols, not \
macros."))))))

(rawmac-add will-defun () () (name out-type &rest arg-types)
  "Allows you to tell a function that you will define in the future.
 (Function inference needs output-type in advance.)"
  (fun-add name arg-types (:state state) :out-type out-type))

(rawmac-add specialize-fun (:code code) () (name &rest args)
  "Forces the function that matches to make a version that is 
specialized to fit the argument types. (Produces error if it doesnt exist.)"
  `(c-defun ,name (,@args)
	    ,@(slot-value (fun-get (car code)
				   (loop for a in args collect (car a))
				   :state state)
			  'full-code)))

(rawmac-add lambda (:code code) () (name (&rest args) &rest body)
  (declare (ignorable name args body))
  (function-creator 'lambda code type-of :state state))

;Fun-of: getting the function of a symbol.
(mac-add fun-of () ((any)) (sym)
;"Defer it to the functions." TODO make one taking the (symbol) argument.
  (values nil :discard))

(mac-add fun-of () ((eql (symbol sym))) (symbol)
  `(,(make-instance 'out :name 'fun-of
       :type `(eql (adapt-fun ,sym)))))

;Funcall
(mac-add funcall () ((eql (adapt-fun sym)))
                    ((fun-of symbol) &rest arguments)
  (unless (eql fun-of 'fun-of)
    (error "Something must have gone wrong in typeset selection.\
 (Could also be stray macro.)"))
  (unless (eql sym symbol)
    (error "Something wrong with type-eql-var?"))
  `(,sym ,@arguments))

(rawmac-add funcall () ((any)) (function &rest arguments)
  (with-fun-resolve
    (let ((fun  (resolve function type-of))
	  (out-type (out-type fun))
	  (args (loop for a in arguments collect (resolve a type-of))))
      (cond ;Constant funcalls are done with the actual function.
	((not (listp out-type))
	 (error "Generic types not supported yet.\
 Also, this should end up of type (adapt-function (symbol)) or more\
 specific."))
      ;See if the function matches.
	((function-match (loop for a in args collect (out-type a))
			 (cddr out-type) :state state)
	 `(,(make-instance 'out :name 'funcall :type (cadr out-type))
	    ,fun ,@args))
	(t
	 (error "Funcall called with nonmatching argument types with\
 function type."))))))

(rawmac-add flet () () ((&rest fundefs) &rest body)
  (with-slots (namespaces write-namespace) state
  ;Trick is to but it in a namespace but one that is not written to.
    (setf write-namespace (gen-c-name state))
    (push write-namespace namespaces)
    (let ((funs ;Now make the functions.
	   (loop for fundef in fundefs
	     collect (function-creator 'flet fundef type-of :state state)))
	  (res (progn ;Flip back namespace writing, resolve.
		 (setf write-namespace (cadr namespaces))
		 (fun-resolve `(progn ,@body) type-of :state state))))
    ;And also flip back reading. (Making the functions inaccessable)
      (pop namespaces)
    ;Result.
      `(,(make-instance 'out :name 'flet :type (out-type res))
	 ,res ,funs))))

(mac-add flet1 () () ((fundef) &rest body)
  `(flet ((,fundef)) ,@body))