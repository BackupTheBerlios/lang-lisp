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

(defpackage #:argument
  (:use #:common-lisp #:generic)
  (:export do-by-argument argumentize-list
	   &key &optional &rest)
  (:documentation "Iterating over a list argument by argument\
 (do-by-argument), and destructuring-bind clone based on it\
 (argumentize-list)"))

(in-package #:argument)

(defun delist (x) (if (listp x) (car x) x))

(defun eql-modulo-keyword (symbol keyword)
  (when (keywordp keyword)
    (string= (symbol-name symbol) (symbol-name keyword))))

(defun do-by-argument (arguments on-list do-fun &optional reverse-chain)
  "Funcalls do-fun analogously with the arguments in the list. The \
function is called element, argument name(including default), the reverse
 of the chain of nth's that get you there, and whether the argument was 
optional. If you get a nil for the argument name, it ran out of arguments, \
and the list did not."
  (let (key have-keys optional)
    (do ((n 0 (+ n 1))
	 (a arguments (cdr a)) (i on-list (cdr i)))
       ;Stop when arguments and non-arguments run out.
	((and (null a) (null i)) nil)
      (flet ((reverse-chain ()
	       (cons n reverse-chain))
	     (getkey ()
	       "Gets the key for some name."
	       (dolist (k key)
		 (when (eql-modulo-keyword (delist k) (car i))
		   (funcall do-fun (cadr i) k reverse-chain '&key)
		   (push (delist k) have-keys)
		   (return)))
	       (setf- cdr i)
	       (setf- + n 1)))
	(cond
	  ((unless (or key optional (null a)) (listp (car a)))
	   (do-by-argument (car a) (car i) do-fun (reverse-chain)))
	  ((eql (car a) '&optional)
	   (setf a (cdr a))
	   (setf optional t)
	   (unless (null a)
	     (funcall do-fun (car i) (car a) (reverse-chain) t)))
	 ;Misses first one, but is keyword. (Not contextsensitive that way.)
	  ((eql (car a) '&key)
	   (setf key (cdr a))
	   (getkey))
	  ((case (car a) ((&rest &body) t))
	   (funcall do-fun i (cadr a) (reverse-chain) (car a))
	   (return))
	  (key
	   (getkey))
	  (t
	   (funcall do-fun (car i) (car a) (reverse-chain)
		    (when optional '&optional))))))
   ;Do the missed keys with default values.
    (dolist (k key)
      (unless (dolist (kh have-keys) (when (eql kh (delist k)) (return t)))
	(funcall do-fun nil k reverse-chain '&key)))))

(defun nth-chain (chain of &key cdr)
  "The chain is a series of indexes of the series of lists, if you follow \
the indexes."
  (if (null chain)
    of `(,(if cdr 'nthcdr 'nth) ,(car chain) ,(nth-chain (cdr chain) of))))

(defun catch-error (value arg chain optional)
  "Meant for in the callback of of do-by-argument, meant to catch some\
 errors."
  (declare (ignorable value chain))
  (cond
    (optional nil) ;;all fine
    ((null arg)    (error "Nonoptional value left out."))
    ((listp arg)   (error "Nonoptional value got default value."))))

(defun sget (list indicator)
  "Special getting for argumentize-list."
  (do ((i list (cddr i)))
      ((or (null i) (eql-modulo-keyword (car i) indicator))
       (car i))))

(defmacro argumentize-list ((&rest arguments) list &body body)
  "Note use cl: destructuring-bind, the only reason i use this one because \
the lang-lisp project will have to detach from common lisp at some point. \
And do-by argument is something common lisp standard stuff doesn't seem to \
provide. (Might be wrong again.)
Setting arguments does not leave the closure."
  (let ((glist (gensym)) vars)
    (do-by-argument arguments nil
      (lambda (useless-value arg chain optional)
	"Collect the nth-chains to get the variables."
	(declare (ignorable useless-value))
	(catch-error useless-value arg chain optional)
	(push `(,(delist arg)
		 ,(case optional
		    (&rest
		     (nth-chain chain glist :cdr t))
		    (&optional
		     (if (listp arg)
		       `(if-use ,(nth-chain chain glist)
				,(cadr arg))
		       (nth-chain chain glist)))
		    (&key
		     (let ((get `(sget ,(nth-chain chain glist :cdr t)
				       ',(delist arg))))
		       (if (listp arg)
			 `(if-use ,get ,(cadr arg))
			 get)))
		    (t
		     (nth-chain chain glist))))
	      vars)))
    `(let ((,glist ,list)) ;Finally, actually make the output.
       (let (,@vars)
	 ,@body))))


