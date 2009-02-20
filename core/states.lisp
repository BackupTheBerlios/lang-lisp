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

;;TODO allow for states to be written at the time of the macro 
;;creation itself?

;Used in to-c.lisp.
(defclass state-to-c ()
  ((nondescript-type-preface
    :initform "Obj" :initarg :nondescript-type-preface)))

(defvar *initial-args*
  '((list-open (list #\( )) (list-close (list #\) ))
    (whitespace (list #\Tab #\Newline #\Space)) (comment (list #\#))
    (except nil) (except-prev-nonsymbol nil)))

(defun set-reader (&optional (to-args *initial-args*))
  "Reads a file by chaining the tokenizer and resolver."
  (lambda (state type-of stream)
    (flet ((get-sym (sym) (copy-list (cadr(assoc sym to-args)))))
      (fun-resolve 
       (tokenize-stream stream
	 :list-open (get-sym 'list-open) :list-close (get-sym 'list-close)
	 :whitespace (get-sym 'whitespace) :except (get-sym 'except)
	 :except-prev-nonsymbol (get-sym 'except-prev-nonsymbol))
       type-of :state state))))

(defclass reader-state ()
  ((reader :initform (set-reader))
   (args :initform *initial-args*)))

(defclass default-state (fun-state state-to-c reader-state)
  ())

(defvar *state* (make-instance 'default-state))