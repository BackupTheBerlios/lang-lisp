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

(mac-add quote () () (symbol)
  (cond
    ((symbolp symbol)
     (make-instance 'value :type `(eql (symbol ,symbol)) :from symbol))
    (t
     (error "quote does not quote anything else then symbols yet."))))

(rawmac-add namespace () () (name &rest body)
  (with-fun-resolve
    (with-slots (namespaces write-namespace) state
      (push name namespaces)
      (setf write-namespace name)
      (let ((out (resolve `(progn ,@body) type-of)))
	(pop namespaces)
	(setf write-namespace (car namespaces))
	out))))

;;Body-like stuff
(rawmac-add progn () () (&rest body)
  (let ((res (loop for c in body
		collect (fun-resolve c type-of :state (print state)))))
    `(,(make-instance 'out :name 'progn :code res
		      :type (out-type(caar(last res))))
       ,@res)))

(rawmac-add let () () ((&rest varlist) &rest body)
  (with-fun-resolve
    (let*((var-list ;List that will later be part of output.
	   (loop for c in varlist
	      collect
		(argumentize-list (name value) c
		  (let ((res (resolve value type-of)))
		    ;Make variable in namespace.
		    (list (intern(with-slots (namespaces) state
			    (if (null namespaces)
			      (if (symbolp name) (symbol-name name) name)
			      (format nil "~D_~D" (car namespaces) name))))
			  res)))))
	  (new-type-of ;Add some types.
	    (append
	     (loop for el in var-list
		collect (list (car el)
			      (out-type (if (listp (cadr el))
					    (caadr el) (cadr el)))))
	     type-of))
	  ;Resolve output. (Doesn't have a progn-like thing at its end.)
	  (out-res  (resolve `(progn ,@body) new-type-of)))
      ;Construct output code.
      `(,(make-instance 'out :name 'let :code out-var
			     :type (out-type (car out-res)))
	 ,var-list
	 ,out-res))))

(mac-add let1 () () ((var to) &rest body)
  `(let ((,var ,to)) ,@body))

;;Loops
(rawmac-add while () () (cond return &rest body)
  (with-fun-resolve
    (let ((out-var (resolve return type-of)))
      `(,(make-instance 'out :name 'while :type '(void))
	 ,(resolve cond type-of)
	 ,(resolve `(progn ,@body) type-of)))))

(mac-add do () () ((&rest vars) (cond return) &rest body)
  `(let (,@(loop for v in vars collect `(,(car v) ,(cadr v))))
     (while ,cond
       ,@body
       ,@(loop for v in vars collect `(set ,(car v) ,(caddr v))))
     ,return))

(mac-add do1 () () ((var start change) (cond return) &rest body)
  `(let ((,var ,start))
     (while ,cond
       ,@body
       (set ,var ,change))
     ,return))

;;Functions
(defun get-var-dependencies (res exclude-args &key (so-far (list nil)))
  (dolist (el res)
    (cond
      ((listp el)
       (get-var-dependencies el exclude-args :so-far so-far))
      ((and* (eql (type-of el) 'value)
	     (not(assoc (from el) exclude-args))
	     (not(in-list so-far (from el))))
       (push el (car so-far)))))
  (car so-far))

(defun function-creator (out-name code type-of &key state)
  "Creates functions. (Used in lang functions flet, defun and lambda.)"
  (with-fun-resolve
  (argumentize-list (name (&rest args) &rest body) (cdr code)
;Explicitly specified out-type.
    (let*((res (resolve `(progn ,@body) (append type-of args)))
	  (arg-types
	   (loop for a in args
	      collect (if (listp a) (cadr a) (list a '(any)))))
         ;Add function.
	  (fun (fun-add name arg-types (:state state)
			:flags (case out-name 
				 (lambda '(:lambda))
				 (flet   '(:flet)))
			:full-code code :code res
			:out-type (out-type(car res)))))
;    ;Return the function. (Although not really meant as used functionally.)
;    ;Lambda would be for that.
      `(,(make-instance 'out :name out-name
			:type `(function ,(out-type(car res))
					 ,@arg-types))
	 ,fun
	 ,(get-var-dependencies res args))))))

(rawmac-add defun (:code code) () (name (&rest args) &rest body)
  (function-creator 'defun code type-of :state state))

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
				   :state state))
	    'full-code))

(rawmac-add lambda (:code code) () (name (&rest args) &rest body)
  (function-creator 'lambda code type-of :state state))

;Fun-of
(mac-add fun-of () ((any)) (sym)
  (error "Do not know type. (Also, arbitrary types not implemented yet.)"))

(mac-add fun-of () ((eql (symbol (any)))) (symbol)
  `((make-instance 'out :name 'fun-of :type `(eql (adapt-fun ,symbol)))))

;Funcall
(rawmac-add funcall () ((eql (adapt-fun (any))))
                    ((fun-of symbol) &rest arguments)
  (unless (eql fun-of 'fun-of)
    (error "Something must have gone wrong in typeset selection.\
 (Could also be stray macro.)"))
  (resolve `(,symbol ,@arguments) type-of))

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
