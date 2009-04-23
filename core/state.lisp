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

;Functions. (Getting them individually in fun-get.lisp)
(defvar *funs* (make-hash-table))

;(raw)Macros. (Getting these individually in mac-get.lisp)
(defvar *macs* (make-hash-table))

(defgeneric get-name (obj of-language)
  (:documentation "Get name of things for different output languages."))

(defmethod get-name (obj (of-lang symbol))
  (getf (slot-value obj 'names) of-lang))

(defmethod (setf get-name) (to obj (of-lang symbol))
  (setf (getf (slot-value obj 'names) of-lang) to))
