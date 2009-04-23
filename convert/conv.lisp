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

(defun conv-code (code conv-macs)
  "Converts lang resolved code into something else.
It is not flexible enough to do C on purpose, the plan is to have\
 transformations on resolved code instead, changing its properties so that\
 this can convert it."
  (if-with mac-fun (gethash (type-of code) conv-macs)
    (funcall mac-fun code conv-macs)
    (error "Couldn't find conversion for a macro output ~D." code)))

(defmacro make-conv (hash name (&rest slots) &body body)
  "Makes making a conv more convenient.
 Makes variables self, code and conv-state, and conv in flet."
  `(setf (gethash ,name ,hash)
	 (lambda (code conv-macs)
	   (flet ((conv (c &optional (conv-macs conv-macs))
		    (conv-code c conv-macs)))
	     (with-slots (,@slots) code
	       ,@body)))))

