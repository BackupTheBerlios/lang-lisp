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

(setf (fun-state-manual-type-coarser *state* '|eql|)
      (lambda (type compare-type state vars)
	"Eql."
	(declare (ignorable state))
	(cond
        ;See if they have the mark of eql.
	  ((or* (not (listp type)) (not (listp compare-type))
		(not (and (eql (car type) '|eql|)
			  (eql (car compare-type) '|eql|))))
	   nil)
	  ((listp (cadr type)) ;General type still undefined eql.
	   (let ((c (cadr compare-type)))
	     (cond ;And so is what we compare to, so they have to match up.
	       ((listp c)
		(and (eql (car c)  (caadr type))
		     (eql (cadr c) (cadadr type))))
	       (t  ;What we compare to is entirely specified, see if in 
					;right class.
		(case (caadr type) ;TODO must be mistake here, c not list.
		  (|integer| (integerp c))
		  (|number|  (numberp c))
		  (|symbol|  (and* (listp c)
				   (eql (car c) '|quote|)
				   (symbolp (cadr c)))))))))
	  (t ;Both entirely set, must match.
	   (eql (cadr type) (cadr compare-type))))))
