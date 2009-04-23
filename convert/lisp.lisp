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

;;TODO Make declarations from info lang has..

(in-package #:lang)

(defvar *conv-lisp-macs* (make-hash-table))

(defmacro conv-body (body)
  `(iter (for el in ,body)
	 (collect (conv el))))

;TODO Ok, how to use lisp via this properly? Want to use common lisps\
; capabilities, rather then redoing it.
; Problem is we don't know how it will be used.
(make-conv *conv-lisp-macs* 'fun (name variants flags args-code body-code)
  (if (in-list flags :is-defun)
    `(function ,(get-name code :lisp)) ;Should have registered.
    (case (length variants)
      (0 '(values)) ;Return nothing, nothing being done with result.
      (1 `(lambda (,@args-code) ;One thing being done with it.
	    ,(conv-code (slot-value (car variants) 'res) conv-macs)))
      (t `(lambda (,@args-code)
  ;Unclear what is being done with it, unfortunately have to see at 
  ;run-time what is done.
	    ,@(conv-code (combine-variants code)))))))

(make-conv *conv-lisp-macs* '-progn (body)
  `(progn ,@(conv-body body)))

(make-conv *conv-lisp-macs* '-let (vars body)
  `(let (,@(iter (for v in vars)
		 (collect `(,(first v)
			     ,(conv (second v))))))
     ,@(conv-body body)))

(make-conv *conv-lisp-macs* 'while (cond body)
  `(do () ((not ,(conv cond)) (values))
     ,@(conv-body body)))

(make-conv *conv-lisp-macs* 'value (from)
  from)

(make-conv *conv-lisp-macs* 'applied-fun (fun args)
  `(,(if-use (if-use (get-name fun :lisp) (get-name fun :usual))
	     (setf (get-name fun :lisp) (gensym))) ;If no name yet, set one.
     ,@(iter (for c in args)
	     (collect (conv c)))))

(defun lisp-conv-fun (fun &optional (conv-macs *conv-lisp-macs*))
  "Converts a single function."
  (with-slots (args-code code) fun
    `(defun ,(get-name fun :lisp)
	 (,@(iter (for a in args-code)
		  (collect (if (listp a) (car a) a))))
       ,(conv-code code conv-macs))))

(defun lisp-conv-all-fun (state &optional (conv-macs *conv-lisp-macs*))
  "Converts all functions in state, outputs them in list."
  (with-slots (funs) state
    (iter (maphash (lambda (key fun)
		     (collect (lisp-conv-fun fun conv-macs)))
		   funs)
	  (finish))))
