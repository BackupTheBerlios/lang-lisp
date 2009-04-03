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

(setf (fun-state-manual-type-coarser *state* '|var|)
      (lambda (general specific state vars)
	"Values are just tags, they don't matter if you're not looking for \
them."
	(flet ((is-value (tp)
		 (and* (listp tp) (case (car tp) (|var| t)))))
	  (cond
	    ((is-value general)
	     (unless (is-value specific) ;If both value, they match anyway.
	       (type-coarser (cadr general) compare-type
			     :state state :vars vars)))
	    ((is-value specific)
	     (type-coarser general (cadr specific)
			   :state state :vars vars))))))
