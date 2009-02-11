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

;;Pointer stuff.
(add-type '|ptr| ('atomic-type) :size
	  (slot-value *state* 'pointer-type-size))

(fun-add '|ptr| '(anything) ()
  :doc-str "Pointer to an object."
  :c-name '& :out-type '(|ptr| anything) :flags '(:chase-args))

(fun-add '|val| '((|ptr| anything)) ()
  :doc-str "Value of an object."
  :c-name '*  :out-type 'anything :flags '(:chase-args))

;;Shifting and differences or pointers.
(fun-add '|aref| '((|integer|) (|ptr| anything)) ()
  :doc-str "Uses C's []" :c-name '[]
  :out-type 'anything :flags '(:chase-args))

(fun-add '|ptr-shift| '((|ptr| anything) (|integer|)) ()
  :doc-str "Shifting a pointer by whole object." :c-name '+
  :out-type '(|ptr| anything) :flags '(:c-binary-fun :chase-args))

(fun-add '|ptr-difference| '((|ptr| anything) (|ptr| anything)) ()
  :doc-str "Difference between pointers.(TODO by whole object??)" :c-name '-
  :out-type '(|ptr-integer|) :flags '(:c-binary-fun :chase-args))

;References are use as if just the argument itself.
(add-type '|ref| ('atomic-type) :size (slot-value *state* 'pointer-type-size))

(push (lambda (type compare-type state)
	(cond
	  ((or (not (listp type)) (not (listp compare-type)))
	   nil)
	  ((null(cddr type))
	   (let ((type-ref (eql (car type) '|ref|))
		 (compare-ref (eql (car compare-type) '|ref|)))
	     (when (or type-ref compare-ref) ;One of them is reference.
	       (type-coarser
		 (if type-ref (cadr type) type)
		 (if compare-ref (cadr compare-type) compare-type)
		 :state state))))))
      (slot-value *state* 'manual-type-generality))

;Note that it used the typeset; when there are objects that are already 
;behind a pointer, but the pointer is never in the users control, you don't
;need to put another pointer behind it; you override this macro.
(fun-add '|ref| '(anything) ()
  :doc-str "Reference to an object, allows you to write to arguments of 
functions to alter things, and have things not be copies of large \
structures, but still use types in (non reference)functions as if they \
weren't references. However, it might be a good idea to stay functional!
WARNING currently ref does _not_ check if what it refers to still exists!"
  :out-type '(|ref| anything) :c-name '&)

(fun-add '|ref| '((|ptr| anything)) ()
  :doc-str "Reference to a pointer. The pointer is made to behave like a \
reference."
  :out-type '(|ref| anything) :c-name 'identity :flags '(:chase-args))

(fun-add '|ptr| '((|ref| anything)) ()
  :doc-str "Pointer of an reference. The reference is already a pointer, \
but now you get to treat it as a pointer too."
  :out-type '(|ptr| anything) :c-name 'identity :flags '(:chase-args))

