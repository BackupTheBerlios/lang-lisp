;;
;;  Copyright (C) 2009-04-03 Jasper den Ouden.
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

(defvar *conv-lisp-macs* (make-hash-table))

(make-conv *conv-lisp-macs* 'progn (&rest body)
  `(progn ,@(iter (for c in body)
		  (collect (conv c)))))

(make-conv *conv-lisp-macs* 'let (vars body)
  `(let (,@(iter (for v in vars)
		 (collect `(,(first v)
			     ,(conv (second v))))))
     ,(conv body)))

(make-conv *conv-lisp-macs* 'while (cond body)
  `(do () ((not ,(conv cond)) (values))
     ,(conv body)))

(make-conv *conv-lisp-macs* 'void-end ()
  '(values))

(defun lisp-conv-fun (code conv-state)
  "Function useage conversion to lisp."
  (let ((fun (car code)))
    `(,(if-use (if-use (get-name fun :lisp) (get-name fun :usual))
	       (setf (get-name fun :lisp) (gensym))) ;If no name yet, set one.
       ,@(iter (for c in (cdr code))
	       (collect (conv-code c conv-state))))))

(defun lisp-conv-value (code conv-state)
  "Value useage conversion to lisp."
  (declare (ignored conv-state))
  (from (car code)))

(defvar *lisp-conv-state*
  (make-conv-state #'lisp-conv-fun #'lisp-conv-value *conv-lisp-macs*)
  "Conversion state for converting to lisp.")

(defun lisp-conv-fun (fun &optional (conv-state *lisp-conv-state*))
  "Converts a single function."
  (with-slots (args-code code) fun
    `(defun ,(get-name fun :lisp)
	 (,@(iter (for a in args-code)
		  (collect (if (listp a) (car a) a))))
       ,(conv-code code conv-state))))

(defun lisp-conv-all-fun (state &optional (conv-state *lisp-conv-state*))
  "Converts all functions in state, outputs them in list."
  (with-slots (funs) state
    (iter (maphash (lambda (key fun)
		     (collect (lisp-conv-fun fun conv-state)))
		   funs)
	  (finish))))
