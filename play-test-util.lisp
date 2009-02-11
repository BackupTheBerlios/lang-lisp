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

(defun summary (input &key more-on-fun more-on-mac)
"Makes the resolved code more readable."
  (flet ((summary (sub-input)
	   (summary sub-input
		    :more-on-fun more-on-fun ::more-on-mac more-on-mac)))
  (case (type-of input)
    (cons
     (loop for el in input collect
	  (summary el)))
    (fun
     (with-slots (name arg-types out-type more-specific) input
       `(:fun ,name ,arg-types ,out-type
	      ,@(cond
		 (more-on-fun
		  `(:more-specific ,(summary more-specific)))))))
    (out
     (with-slots (name out-type code) input
       (append (list :out name out-type)
	       (when more-on-mac `(:code ,(summary code))))))
    (value
     (with-slots (out-type from) input
       `(:val ,(summary from) ,out-type)))
    (t
     input))))

(defmacro print-summary (input &rest stuff)
  "Prints the result from list-summary."
  `(print(summary ,input ,@stuff)))
