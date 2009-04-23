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

(mac-add |defun| () (name (&rest args) &rest body)
  `(|set| (|fun-of| (|quote| ,name))
	  (|named-lambda| ,name (,@args)
	     ,@body)))

(mac-add |set| (:*local local) (to-set to-val)
  (cond
    ((listp to-set)
     (cond 
       ((and (eql (car to-set) '|fun-of|)
	     (eql (caadr to-set) '|quote|))
	(let ((res (all-resolve local to-val)))
;	  (unless (function-p res)
;	    (error "Direct s-expression functions; as given by\
; (fun-of symbol) must be functions."))
	  (setf (fun-get local (cadadr to-set))
		res)))))))

(defun read-declaration (declarations local args body)
  (let (variants flags always-inline out-type-fn)
    (dolist (el declarations) ;Iterate all the declarations.
      (case (car el)
	(|specify| ;Make more specified variants based on declaration.
	 (unless always-inline
	   (push (make-variant
		  local (if-use (cadr el) (gensym)) args (cddr el) body)
		 variants)))
	(|out-type-fn| ;Provide function that calculates the resulting-type.
	 (setf out-type-fn ;Newer ones override older ones. (TODO warnings?)
	       (all-resolve local (cadr el))))
	(|flags| ;Provide flags in bulk.
	 (dolist (f (cdr el))
	   (when (eql f '|:always-inline|)
	     (setf always-inline t)
	     (setf variants nil)))
	 (setf- append flags (cdr el)))
	(|always-inline| ;Inline in every case. (Makes variants useless.)
	 (setf always-inline t)
	 (setf variants nil)
	 (push '|:always-inline| flags))
	(|otherwise-inline| ;Inline if specified not found.
	 (push '|:otherwise-inline| flags))))
    (values variants flags always-inline out-type-fn)))

(mac-add |named-lambda| (:*local local) (name (&rest args) &rest body)
  "Raw version of lambda."
  (let ((doc-str (when (stringp (car body)) (car body))))
    (when (stringp (car body))
      (setf- cdr body))
    (multiple-value-bind (variants flags always-inline out-type-fn)
	   (when (and (listp (car body)) (eql (caar body) '|declare|))
	     (read-declarations (cdar body) local args body))
      (make-instance 'fun :name name 
        :doc-str doc-str :out-type-fn out-type-fn
	:args-code args :body-code body
	:variants variants
	:flags flags))))

(mac-add |lambda| () ((&rest args) &rest body)
  "Creation of anonymous functions."
  `(|named-lambda| ,(gensym) (,@args) ,@body))

(mac-add |declare-for| (:*local local) (for &rest declarations)
  "Declaring things for functions and such.(TODO function-p)"
  (let ((res (all-resolve local for)))
    (cond
      ((function-p res)
       (let ((fun (fun-via res)))
	 (with-slots (args-code body-code variants flags) fun
	   (multiple-value-bind (n-variants n-flags always-inline 
				 n-out-type-fn)
	       (read-declarations declarations local args-code body-code)
	     (setf- append variants n-variants)
	     (when always-inline ;Always inline makes variants moot.
	       (setf variants nil))
	     (setf- append flags n-flags)
	     (when n-out-type-fn
	       (setf out-type-fn n-out-type-fn))))))
      (t
       (error "Did not recognize the thing it declaring for.")))))

(mac-add |funcall| (:*local local) (fun &rest args)
  "Uses function with the arguments. Arguments must be right count"
  (fun-resolve local (all-resolve local fun) args))
