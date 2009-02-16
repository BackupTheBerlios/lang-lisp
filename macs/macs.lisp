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
     (make-instance 'value :type `(eql ,symbol) :from symbol))
    (t
     (error "quote does not quote anything else then symbols yet."))))

(mac-add const () () (const)
  (make-instance 'value :type `(|eql| ,const)))

(rawmac-add namespace () () (name &rest body)
  (with-fun-resolve
    (with-slots (namespaces write-namespace) state
      (push name namespaces)
      (let ((wrote-namespace write-namespace))
	(setf write-namespace name)
	(let ((out (resolve `(progn ,@body) type-of)))
	  (pop namespaces)
	  (setf write-namespace wrote-namespace)
	  out)))))

(mac-add progn () () (&rest body)
  (case (length body)
    (1 (car body))
    (t `(progn-raw ,@body))))
    
;;Body-like stuff
(rawmac-add progn-raw () () (&rest body)
  "A function body. All things with function bodies pass through here."
  (setf body (loop for b in body
		if (and* (listp b) (case (car b) ((progn |progn|) t)))
		append (cdr b)
		else
		collect b))
  (let ((res (loop for c in body
		collect (fun-resolve c type-of :state state))))
    `(,(make-instance 'out :name 'progn :code res
		      :type (out-type(car(last res))))
       ,@res)))

(rawmac-add let () () ((&rest varlist) &rest body)
  "Makes variables. Made in sequence."
  (with-fun-resolve
    (let*((var-list ;List that will later be part of output.
	   (loop for c in varlist
	      collect
		(argumentize-list (name value) c
		  (let ((res (resolve value type-of)))
		    ;Make variable in namespace.
		    `(,(namespace-symbol name state) ,res)))))
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
      (if (null var-list)
	  (progn (warning "Style: only one argument in a res!")
		 out-res)
	  `(,(make-instance 'out :name 'let :code out-res
			    :type (out-type (car out-res)))
	     ,var-list
	     ,out-res)))))

(mac-add let1 () () ((var to) &rest body)
  `(let ((,var ,to)) ,@body))


(mac-add let-ret () () ((ret-var to) &rest body)
  "Let1, but returns the variable."
  `(let ((,ret-var ,to)) ,@body ,ret-var))
