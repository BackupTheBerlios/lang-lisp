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

;Just a typeset, only via a hash table. (via get-symbol)

(defclass typeset-named (typeset) ;Not used in functions, but ah well..
  ((name :initarg :name :initform nil)))

(defun named-typeset-get (name arg-types hash &key state)
  "Gets a typeset object."
  (if-with sym (get-symbol name hash state)
    (typeset-get sym arg-types :state state)
    (error "named-typeset-get couldn't find a macro/function under this \
name. ")))

(defun (setf named-typeset-get) (to name arg-types hash
				 &key (state *state*))
  "Adds a typeset object.(Does not replace old object unless exact!)"
  (if-with got (get-symbol name hash state)
      (setf (typeset-get got arg-types :state state) to)
      (setf (get-symbol name hash state)
	    (make-instance 'typeset :more-specific (list to)))))
