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
  ((var-precede :initform "" :initarg :var-precede)

   (nondescript-type-preface
    :initform "Obj" :initarg :nondescript-type-preface)))

(defvar *initial-args*
  '((list-open (list #\( )) (list-close (list #\) ))
    (whitespace (list #\Tab #\Newline #\Space)) (comment (list #\#))
    (except nil) (except-prev-nonsymbol nil)))

(defclass reader-state ()
  ((reader :initform nil) ;functional (&optional (to-args *initial-args*))
   (args :initform *initial-args*)))

(defclass default-state (fun-state state-to-c reader-state)
  ())

(setf *state* (make-instance 'default-state))
