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
