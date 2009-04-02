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

;;Loops
(rawmac-add raw-while () () (cond &rest body)
  "Same as while, but no optional returing of value. Made it so that\
 conversion is easier."
  `(,(make-instance 'out :name 'while :type '(|void|))
     ,(fun-resolve cond type-of)
     ,(fun-resolve `(progn-raw ,@body) type-of)))

(mac-add while () () ((cond &optional return) &rest body)
  "Does something while condition true."
  (if return
    `(progn (raw-while ,cond ,@body)
	    ,return)
    `(raw-while ,cond ,@body)))

(mac-add do () () ((&rest vars) (cond &optional return) &rest body)
  "Same do as that of lisp.(However variables made in sequence.)"
  `(let (,@(loop for v in vars collect `(,(car v) ,(cadr v))))
     (while (,cond ,return)
       ,@body
       ,@(loop for v in vars collect `(set ,(car v) ,(caddr v))))))

(mac-add do1 () () ((var start change) (cond return) &rest body)
  "'Do with one variable."
  `(let1 (,var ,start)
     (while (,cond ,return)
       ,@body
       (set ,var ,change))))
;
;(mac-add do-times () ((|integer|)) (count (var &optional return) &rest body)
;  "Does something count times."
;  `(do1 (,var 0 (+ ,var 1))
;	((< ,var ,count) ,return)
;     ,@body))

(mac-add do-times () ((|eql| (|integer| n)))
    (count (var &optional return limit-count) &rest body)
  "Does something count times, limit-count determines up to what count the \
do-times does the operation without loop."
  ;Default limit-count completely arbitrary.
  (declare (ignorable count))
  (if (> n (if-use limit-count 4))
    `(do1 (,var 0 (+ ,var 1))
	  ((< ,var ,n) ,return)
       ,@body)
    `(progn ;Manually.
       ,@(loop for i from 0 upto (- n 1)
	    collect `(let ((,var (const ,i))) ,@body))
       ,return)))
