
;;
;;  Copyright (C) 2009-04-08 Jasper den Ouden.
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

;Simple values like from constants and variables go in these.
(defclass value ()
  ((from :initarg :from :initform nil :accessor from)
   (val :initarg :val :initform nil)
   (out-type :initarg :type :initform nil :accessor out-type))
  (:documentation "Holds the information of values in Lang."))

(defmacro with-local (local &body body)
  "Useful shortcuts when working with local stuff."
  (with-gensyms (*local)
    `(let ((,*local local))
       (flet ((get-local (of-slot name)
		(symget ,*local (list(slot-value ,*local of-slot))
				 name))
	      (resolve (code &key defer-to-fun
			     (pre (lambda (local code) code))
			     (post (lambda (local code) code)))
		(all-resolve ,*local code
			     :defer-to-fun defer-to-fun
			     :pre pre :post post)))
	 ,@body))))

(defun macro-resolve (local mac code)
  "Resolves a macro."
  (when mac
    (with-local local
      (multiple-value-bind (result validity)
	  (funcall (slot-value mac 'fun) code local)
	(case validity ;Take messages from the macro.
	  (:defer-to-fun ;Do the result with only functions.
					; (For functions on the same name.
	   (resolve result :defer-to-fun t))
	  (:discard ;Discard result an defer to function instead.
	   (resolve code :defer-to-fun t))
	  (:again ;Resolve again.
	   (resolve result))
	  (:is-done
	   result)
	  (t
	   (resolve result)))))))

(defclass applied-fun ()
  ((fun :type function :initarg :fun)
   (args :initform nil :initarg :args :type list)
   (out-type :initarg :out-type :initform nil :accessor out-type))
  (:documentation "Non-standard function useage."))

(defun apply-fun (fun args variant &optional out-type)
  (make-instance 'applied-fun :fun fun :args args :out-type out-type))

(defun make-variant (local name args types code &key names)
  (make-instance 'fun-variant :arg-types types :names names 
    :res (iter
	   (for c in code)
	   (collect
	       (all-resolve (local-var-with-types local args types) c)))))

(defun fun-resolve (local fun args)
  "Resolves the function."
  (let*((arguments (iter (for a in args)
			 (collect (all-resolve local a))))
	(types     (iter (for a in arguments)
			 (collect (out-type a)))))
  (with-slots (variants flags body-code) fun
    (if-with matching (dolist (v variants)
			(when (satisfying-subset
			       types (slot-value v 'arg-types))
			  (return v)))
      (apply-fun fun arguments matching ;Use existing resolvation.
		 (out-type (slot-value matching 'res)))
      (if (null body-code)
	(apply-fun fun arguments nil ;Base function, no resolving.
		   (with-slots (out-type-fn) fun
		     (when out-type-fn
		       (funcall out-type-fn fun types))))
	;TODO needs to be cleverer? Make the variant a larger subset that
        ; can be computed as easily?
	(let ((variant (make-variant ;Need new version of function.
			local (gensym) (slot-value fun 'args-code) types
			(slot-value fun 'body-code))))  ;Make a new variant.
	  (push variant (slot-value fun 'variants))
	  (apply-fun fun arguments variant
		     (out-type (car(last(slot-value variant 'res)))))))))))

(defmacro if-return (&rest rest)
  "Runs upto and returns the first expression that is non-nil."
  `(if-use ,(car rest) ,(if (null (cddr rest))
			    (cadr rest)
			    `(if-return ,@(cdr rest)))))

(defun value-resolve (local code &key (pre (lambda (local code) code))
		                      (post (lambda (local code) code)))
  "Resolves values of expressions."
  (with-local local
    (cond ;NOTE Make sure that this is from specific to general.
      ((numberp code)
       (make-instance 'value :from code :val code :type `(|eql| ,code)))
      ((symbolp code) ;TODO this clause never reached, prolly.
       (error "Was wrong in thinking this never reached.")
       (let ((val (resolve code :pre pre :post post)))
	 (make-instance 'value :from code :out-type (out-type val)
			:val val))))))

(defun all-resolve (local code &key defer-to-fun
		    (pre (lambda (local code) code))
		    (post (lambda (local code) code)))
  "Resolves all aspects."
  (with-local local 
   ;Let the pre-function work on it.
    (setf code (funcall pre local code))
   ;And the post-function.
    (funcall post local
      (cond
	((null code)
	 nil)
	((not (listp code)) ;Not a list.
	 (cond  ;Must already have been processed as a value/body.
	   ((case (type-of code)
	      ((fun applied-fun -progn -let -flet) t))
	    code)
	   ((symbolp code);Either it is a constant, or it is a symbol-macro.
	    (if-use
	     (get-local 'sym-macros code)
	     (if-with val (get-local 'variables code)
	       (make-instance 'value :from code
				     :val val :type (out-type val))
	       (error "(lang)Variable ~D is unbound" code))))
	   (t
	    (value-resolve local code))))
	((not (symbolp (car code)))
	 (error "First element of s-expression should be symbol.
It is ~D" (car code)))
	(t ;Either some macro or some function.
	 (let ((name (car code)))
	   (if-return
	    (macro-resolve local (get-local 'macros name) code)
	    (macro-resolve local (symget local *macs* name) code)
	    (fun-resolve
	     local
	     (if-return
	      (get-local 'functions name)
	      (fun-get local name)
	      (error "Couldn't find any function or macro matching this \
name ~D" name))
	     (cdr code)))))))))