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

(defun type-fill (type with-el)
  "Uses the type specification and the elements as arguments to produce the
type that it actually is."
  (case (type-of type)
    (symbol
     (cadr (assoc type with-el)))
    (cons
     (cons (car type)
	   (loop for tp in (cdr type)
	      collect (type-fill tp with-el))))
    (t
     (error (format nil "Incorrect type. ~D" type)))))

(defun typelist-fill (types with-el)
  (loop for tp in types collect (type-fill tp with-el)))

(defun type-get-var (general specific &key (state *state*) 
		     (do-types t) (do-eql t) (got-var (list nil)))
  "See typelist-get-var."
  (cond
    ((symbolp general)
     (cond
       ((assoc general (car got-var))
	(unless (equal specific (cadr(assoc general (car got-var))))
	  (error "Mismatch of type, these two types should be the same.")))
       (do-types (push (list general specific) (car got-var)))))
    ((not(listp general))
     (error "Erronous type."))
    ((eql (car general) '|eql|)
     (when do-eql
       (argumentize-list ((type var)) (cdr general)
       (argumentize-list (s-eql eql-to) specific
	 (unless (eql s-eql '|eql|)
	   (error "These don't match, they should have."))
	 (unless (case type (|symbol|  (symbolp eql-to))
		            (|number|  (numberp eql-to))
			    (|integer| (integerp eql-to)))
	   (error "Eql type not of type requested, or not implemented."))
	 (unless (or* (null (assoc var (car got-var)))
		      (eql eql-to (cadr (assoc var (car got-var)))))
	   (error "Mismatch of eql, these two types should be the same."))
	 (push (list var eql-to) (car got-var))))))
    (t
     (loop for g in (cdr general)
	   for s in (cdr specific)
       do (type-get-var g s :state state
	    :do-types do-types :do-eql do-eql :got-var got-var))))
  (car got-var))

(defun typelist-get-var (general specific
			 &key (state *state*) (do-types t) (do-eql t)
			 (got-var (list nil)))
  "Basically the reverse of type-fill, gets the symbol values that are 
filled in."
  (loop for g in general
        for s in specific
     do (type-get-var g s :state state :do-types do-types :do-eql do-eql
		      :got-var got-var))
  (car got-var))

(defun type-list-var (type &optional have)
  "gets the variables in a type."
  (cond
    ((symbolp type) (if (in-list have type) have (cons type have)))
    ((listp type)   (dolist (tp (cdr type))
		      (setf have (type-list-var tp have)))
                    have)
    (t              have)))

(defun typelist-list-var (typelist &optional have)
  (if (null typelist) have
    (typelist-list-var (cdr typelist) (type-list-var (car typelist)))))
    

(defgeneric out-type (of-thing))
;Stuff must match if the out-type is not applicable.
(defmethod out-type (a-priory)
  nil)

(defmethod out-type ((list list))
  (let ((got (car list)))
    (case (type-of got)
      (fun ;Get the variables in the type.
       (if (in-list (slot-value got 'flags) :chase-args)
	 (typelist-fill (out-type got)
			(typelist-get-var (out-type got)
					  (loop for arg in (cdr list)
					     collect (out-type arg))))
	 (out-type got)))
      (t ;Take as-is.
       (out-type got)))))
