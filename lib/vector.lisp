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

(with-slots (write-namespace namespaces load-lib-actions) *state*
  (setf write-namespace '|vector|)
  (setf namespaces '(|vector|))
  (setf (gethash '|vector| load-lib-actions)
	(lambda (type-of state)
	  )))

(fun-resolve
 `(progn
    ;Adding, substracting
    ,@(loop for fun in '(+ -) collect
	`(defun ,fun :only-record :specify-as-used :functional
  	     ((a (array1 (integer) (any))) (b (array1 (integer) (any))))
	     (let ((sum (make-array1 (length a))))
	      (do-array1 (a b sum) (i el-a el-b el-sum)
		(set el-sum (,fun el-a el-b)))
	      sum)))
   ;Multiplying, dividing.
    ,@(loop for fun in '(* /) collect
	`(defun ,fun :only-record :specify-as-used :functional
  	     ((vec (|array1| (|integer|) (any))) (div (number)))
	   (let ((ret (make-array1 (length vec))))
	     (do-array1 (vec ret) (i el-vec el-ret)
	       (set ret (,fun vec div)))
	     ret)))
   ;Change with +,-
    ,@(loop for fun in '(set-+ set--) collect
	`(defun ,fun :only-record :specify-as-used
	     ((alter  (ref(array1 (integer) (any))))
	      (change (array1 (integer) (any))))
	   (do-array1 (alter change) (i alter-el change-el)
	     (set alter-el (,fun alter-el change-el)))
	   alter))
    ,@(loop for fun in '(set-* set-/) collect
	`(defun ,fun :only-record :specify-as-used
	     ((alter  (ref(array1 (integer) (any))))
	      (change (number)))
	   (do-array1 (alter) (i alter-el)
	     (set alter-el (,fun alter-el change)))
	   alter))
   ;Inproduct, length.
    (defun |inpr| ((vec-a (|array1| (|integer|) (any)))
		   (vec-b (|array1| (|integer|) (any))))
      (let ((sum 0))
	(do-array1 (vec-a vec-b) (i el-a el-b)
	  (set sum (+ sum (* el-a el-b)))))))
    (defun |lensqr| ((vec (|array1| (|integer|) (any))))
      (let ((sum 0))
	(do-array1 (vec) (i el) (set sum (+ sum (sqr el))))
	sum))
    (defun |len| (vec) (|sqrt| (|lensqr| vec)))
 '())




