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

(defun test-1 (&key (end-chance 0.1) (maxlen 4) (maxdepth 4)
	       (from-symbols '(a b c d e f g h i j k l m n o p
			       q r s u v w x y z)))
  "Tests whether getting variables in types and putting them there is
bijective, as should be. Currently does not check |eql| or other special 
stuff."
  (let*((base-type   (random-tree end-chance maxlen maxdepth
				  :from-symbols from-symbols))
	(arguments   (loop for el in from-symbols
			collect `(,el (gensym))))
	(filled-type (type-fill base-type arguments))
	(got-arguments (type-get-var base-type filled-type)))
    (values
     (loop for a in got-arguments
	unless (eql (cadr (assoc (car a) arguments)) (cadr a))
	collect `(:missed ,a :vs ,(assoc (car a) arguments)))
     base-type arguments filled-type got-arguments)))

(defun test (cnt &key (end-chance 0.1) (maxlen 4) (maxdepth 4)
	     (from-symbols '(a b c d e f g h i j k l m n o p
			     q r s u v w x y z)))
  "See test-1."
  (loop repeat cnt
    append (test-1 :end-chance end-chance :maxlen maxlen :maxdepth maxdepth
		   :from-symbols from-symbols)))

;Do the test.
(test 1000)





