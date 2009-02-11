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

(rawmac-add aref () ((|integer|) (|array1| (|eql| (|integer| n)) item))
	    (index array)
  "Gets array elements for arrays with (eql (integer n)) length."
  (with-fun-resolve 
   `(,(make-instance 'out :name 'aref-array1-eql :type item)
      ,(resolve index type-of) ,(fun-resolve array type-of))))

(mac-add aref () ((|integer|) (|array| (|integer|) item)) (index array)
  "Getting array elements for non-eql array lengths deferred to functions."
  (values nil :discard))


(mac-add array1-do () () ((&rest arrays) (index &rest elements) &rest body)
  "Does body for elements of the array, the minimum of the lengths counts."
  (let ((letvar (loop for a in array collect `(,(gensym) ,a))))
  `(let (,@letvar)
     (do-times (min ,@(loop for var in letvar collect
			   `(get-slot '|len| ,var)))
       (,index ,return)
       ,@(if (null elements)
	     body
	     `((let(,@(loop for el in elements
			    for var in letvar
			 collect `(,el (ref (aref ,index ,(car var))))))
		 ,@body)))))))

(fun-add 'arr-ptr '((|array1| (|eql| (|integer| n)) item)) ()
  :out-type '(ptr item) :flags '(:chase-args))

(load-file "array1.lang")
