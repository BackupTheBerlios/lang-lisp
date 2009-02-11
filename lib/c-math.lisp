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
  (setf write-namespace '|c-math|)
  (setf namespaces nil)
  (setf (gethash |math.lisp| load-lib-actions)
	(lambda (type-of state)
	  )))

;All the math.h functions.
(dolist (number '(|long-double| |double| |float|))
  (dolist (fun-name '(|cos| |sin| |tan| |acos| |asin| |atan| |atan2|
		      |cosh| |sinh| |tanh|
		      |exp| |log| |log10|
		      |sqrt| |fabs| |floor|))
    (fun-add fun-name `((,number)) :out-type `(,number)))
  (fun-add '|pow|  `((,number) (,number)) :out-type `(,number))
  (fun-add '|fmod| `((,number) (,number)) :out-type `(,number))
  (fun-add '|modf| `((,number) (|ptr| (,number))) :out-type `(,number))
  
  (fun-add '|idexp| `((,number) (|int|)) :out-type `(,number))
  (fun-add '|frexp| `((,number) (|ptr| (|int|))) :out-type `(,number)))
  
(fun-add '|pow| '((|double|) ((|int|))) :out-type '(|double|))
(fun-add '|pow| '((|long-double|) ((|int|))) :out-type '(|long-double|))

(fun-resolve
 `(progn
  ;And this how we add the constants:
   (defun |:inline| |:only-record| |exp| ((x (eql 1)))
     ,(exp 1))
   (defun |:inline| |:only-record| |log| ((x (eql 2)))
     ,(log 2))
   (defun |:inline| |:only-record| |pi| ()
     ,pi)
  ;And just value/reference instead of pointer for modf and frexp.
   (defun |modf| |:inline| |:only-record|
	    (numerator denomenator)
     (|modf| denomenator (|ptr| denomenator)))
   (defun |frexp| |:inline| |:only-record|
	    (numerator denomenator)
     (|frexp| denomenator (|ptr| denomenator)))
  ;Other-base logaritms.
   (defun |log| |:only-record| |:specify-as-used| (number base)
     "Logaritm with base."
     (/ (log number) (log base)))
   (defun |log| (number (base (eql 10)))
     "Uses the log10 from math.h"
     (|log10| number))
     
   (defun % |:inline| |:only-record| (numerator denomerator)
     "Modulo for noninteger values.(Integer version in\
 types/number-types.lisp)."
     (|fmod| numberator denomerator))))
