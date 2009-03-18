;
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


;;TODO see if any good libs for this around.

(in-package #:lang)

(defvar *default-conv* (list nil))

(defun get-conv (from to &optional (convs *default-conv*))
  "Chooses conv function based on two symbols that name them."
  (getf (getf (car convs) from) to))

(defun (setf get-conv) (fun from to &optional (convs *default-conv*))
  "Sets conv based on symbols that name them. Will replace if exist."
  (setf (getf (getf (car convs) from) to) fun))

(defmacro def-conv (name (from to &key (input 'input) (rest 'rest)
			       (convs '*default-conv*))
		   &body body)
  "Makes a conv. If name is non-nil it will also be a function on that \
name. (Otherwise function is on gensym.) Documentation string allowed."
  (setf- if-use name (gensym))
  `(progn (defun ,name (,input ,rest)
	    (declare (ignorable rest))
	    ,@body)
	  (setf (get-conv ',from ',to ,convs) #',name)))

(defun evalf (from to input rest &optional (convs *default-conv*))
  "Evaluates from one point to another. WARNING currently you need to 
provide all chains you use explicitly with function chain-convs."
  (if-with fun (get-conv from to convs)
    (funcall fun input rest)
    (error (format nil "Evalf did not find conv from ~D to ~D."
		   from to))))

;;Hmm, want any more? Wish i knew graph theorems.
(defun chain-convs (chain &optional (convs *default-conv*))
  "Sees if there are links missing, creates them where possible."
  (unless (null (cddr chain))
    (do ((i (cdr chain) (cdr i))) ((null (cdr i)) nil)
      (unless (get-conv (car chain) (cadr i) convs)
	(setf (get-conv (car chain) (cadr i) convs)
	  (let ((from (car chain)) (intermediate (car i))
		(to (cadr i))) ;Witout let, it stays linked to chain and i!!
	    (lambda (input rest)
	      ;Assume we have the one from (car chain) to (car i) and
	      ; (car i) to (cadr i)
	      (evalf intermediate to
		(evalf from intermediate input rest convs)
		rest convs))))))
    (chain-convs (cdr chain) convs)))

(defmacro evalm (from to input &rest rest)
  "See evalf."
  `(evalf ',from ',to ,input (list ,@rest) *default-conv*))
