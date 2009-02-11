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
	      collect (fill-type tp with-el))))
    (t
     (error "Incorrect type."))))

(defun typelist-fill (types with-el)
  (loop for tp in types collect (type-fill tp with-el)))

(defun typelist-get-var (general specific
			 &key (state *state*) (do-types t) (do-eql t))
  "Basically the reverse of type-fill, gets the symbol values that are 
filled in."
  (let (got-var)
    (loop for g in general
          for s in specific do
       (cond
	 ((symbolp g)
	  (when do-types (push (list g s) got-var)))
	 ((and* (listp g) (eql (car g) '|eql|))
	  (when do-eql
	    (argumentize-list ((type var)) (cdr g)
	    (argumentize-list (s-eql eql-to) s
	      (unless (eql s-eql '|eql|)
		(error "These don't match, they should."))
	      (unless (case type (|symbol|  (symbolp eql-to))
			         (|number|  (numberp eql-to))
				 (|integer| (integerp eql-to)))
		(error "Eql type not of type requested, or not implemented."))
	      (push (list var eql-to) got-var)))))
	 (t
	  (setf- append got-var (type-get-var g s :state state)))))
    got-var))

(defun type-get-var (general specific &key (state *state*))
  "See typelist-get-var."
  (typelist-get-var (cdr general) (cdr specific) :state state))

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
