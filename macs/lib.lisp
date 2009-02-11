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

(defun load-file (str &optional (state *state*))
  "Loads the file."
  (cond
    ;In common lisp form.
    ((or (string= (subseq str (- (length str) -5)) ".lisp")
	 (string= (subseq str (- (length str) -5)) ".LISP"))
     (load str))
    ;In silisp form.
    ((string= (subseq str (- (length str) -7)) ".lang")
     (with-open-file (read-from str :direction :input)
       (funcall (slot-value state 'reader) state read-from)))
    (t
     (error "Don't recognize file type."))))

(rawmac-add load-lib () ((eql (symbol (any)))) ((quote symbol))
  (unless (eql quote 'quote)
    (error "(langs fault)Something must have gone wrong in typeset."))
  (with-slots (namespaces write-namespace load-lib-actions) state
    (let ((save-namespaces namespaces) ;Flip namespace data back and forth.
	  (save-write-namespace write-namespace))
      (setf namespaces nil)
      (setf write-namespace nil)
      (load-file (symbol-name symbol) state) ;Read the file.
      (setf namespaces save-namespaces)
      (setf write-namespace save-write-namespace))
    (funcall (gethash symbol load-lib-actions) type-of state)))


