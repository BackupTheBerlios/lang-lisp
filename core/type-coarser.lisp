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

(defun type-coarser (type specific &key (state *state*) (vars (list nil))
		     no-conversion)
  "function-match for a single argument."
  (cond
   ;Symbols that are not first in list are variables to first 
   ;which type it is. '(any) is unnamed arbitrary.
    ((symbolp type) ;See if correct, or first appearance.
     (if-with got (assoc type (car vars))
       (equalp (cadr got) specific)
       (progn (push (list type specific) (car vars))
	      t)))
   ;Can manually manipulate to make things more/less general.
    ((loop for coarser-fun in (slot-value state 'manual-type-coarser)
	when (funcall coarser-fun type specific state vars)
	return t)
     t)
    ((not (listp specific))
     nil)
   ;Any type not a list nor a symbol is a problem, 
    ((not (listp type))
     (error "Types should be lists or symbols.")
     nil)
    ;Numbers, integers, etc. are checked for precise equality.
;TODO eql* also checks for variable numbers? (Code for that elsewhere.)
    ((eql (car type) (car specific))
     (when (= (length type) (length specific))
       (not (loop for fatp in (cdr type)
	          for tp in (cdr specific)
	       unless (type-coarser fatp tp :state state :vars vars
				    :no-conversion no-conversion)
	       return t))))
    (t nil)))

;;TODO how to make and, or?
(defun type-list-coarser (general specific &key (state *state*)
			  (vars (list nil)) no-conversion)
  "Determines whether the given list of types is such that it can be used 
to form the argument of the function."
  (when (= (length general) (length specific))
    (not (loop for g   in general
 	       for s in specific
	    unless (type-coarser g s :state state :vars vars
				     :no-conversion no-conversion)
	    return t))))

(defun type-eql (t1 t2 &key (state *state*))
  (and (type-coarser t1 t2 :state state)
       (type-coarser t2 t1 :state state)))

(defun type-list-eql (tl1 tl2 &key (state *state*))
  (and (type-list-coarser tl1 tl2 :state state)
       (type-list-coarser tl2 tl1 :state state)))
