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

(defun random-tree (end-chance maxlen maxdepth &key top
		    (from-symbols '(a b c d e f g h i j k l m n o p
				    q r s t u v w x y z)))
  "Creates a random tree."
  (flet ((gen-sym ()
	   (nth (random (length from-symbols)) from-symbols)))
    (cond
      ((and (not top)
	    (or (= maxdepth 0) (null end-chance)
		(< (random 1.0) (if (listp end-chance)
				    (car end-chance) end-chance))))
       (gen-sym)) ;Note that it is immaterial what the symbols are, except 
                  ;for the special ones.
      (t
       (let ((out (loop repeat (max 1 (random maxlen)) collect
		       (random-tree
			(if (listp end-chance) (cdr end-chance) end-chance)
			maxlen (- maxdepth 1)))))
	 (if top out (cons (gen-sym) out)))))))

