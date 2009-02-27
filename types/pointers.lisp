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
	  (get-extension-slot *state* :types 'pointer-type-size))

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

;References are used as if just the argument itself.
(add-type '|ref| ('atomic-type)
	  :size (get-extension-slot *state* :types 'pointer-type-size))

(setf (fun-state-manual-type-coarser *state* '|ref|)
      (lambda (type compare-type state vars)
	(flet ((reference (tp)
		 (and* (listp tp) (= 2 (length tp))
		       (case (car tp) ((|ref| |ref-var|) t)))))
	  (when (reference compare-type)
	    (cond
	      ((reference type)
	   ;Ref-var subordinate to ref to keep ordering of types.
	       (unless (eql (car type) '|ref-var|)
		 (type-coarser (cadr type) (cadr compare-type)
			       :state state :vars vars)))
	      (t
	       (type-coarser type (cadr compare-type)
			     :state state :vars vars)))))))

;TODO what about references if setf-functions and variables, surely treat 
;them differently. (call then ref-var and ref-fun, ref-var convertable to 
;functions.)

;Note that it used the typeset; when there are objects that are already 
;behind a pointer, but the pointer is never in the users control, you don't
;need to put another pointer behind it; you override this macro.
(fun-add '|ref| '(anything) ()
  :doc-str "Reference to an object, allows you to write to arguments of 
functions to alter things, and have things not be copies of large \
structures, but still use types in (non reference)functions as if they \
weren't references. However, it might be a good idea to stay functional!
WARNING currently ref does _not_ check if what it refers to still exists!"
  :out-type '(|ref| anything) :c-name '& :flags '(:chase-args))

;Only one level of reference allowed.
(evalm str res "progn-raw
  (defun ref :inline (of (ref(ref anything));)
    of))")

(fun-add '|ref| '((|ptr| anything)) ()
  :doc-str "Reference to a pointer. The pointer is made to behave like a \
reference."
  :out-type '(|ref| anything) :c-name 'identity :flags '(:chase-args))

(fun-add '|ptr| '((|ref| anything)) ()
  :doc-str "Pointer of an reference. The reference is already a pointer, \
but now you get to treat it as a pointer too."
  :out-type '(|ptr| anything) :c-name 'identity :flags '(:chase-args))


(fun-add '|ref| '((|var| anything)) ()
  :doc-str "Reference to a variable; does not do anything, but allows it to 
  automatically reference when there is a reference-only function."
  :c-name 'identity :flags '(:chase-args)
  :out-type '(|ref-var| anything))

(fun-add '|ptr| '((|ref-var| anything)) ()
  :doc-str "Pointer of a referenced variable."
  :c-name '& :flags '(:chase-args)
  :out-type '(|ptr| anything))
