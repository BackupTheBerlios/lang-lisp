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

;Handy macros.
(load "generic.lisp")
;Reading s-expresions from file.
(load "reader.lisp")

(defpackage #:lang
  (:use #:common-lisp #:generic #:reader))

;Function and macro to get stuff from a list like a macro.
(load "argument-positioner.lisp")

;The code that does the processing to an s-expression with the functions and
;values entirely specified as objects with arguments and return types.
(load "fun-base.lisp")
(load "states.lisp")  ;Extensions of the state that macros/output use.

(load "typeset.lisp")
(load "type-util.lisp")

(load "typeset-named.lisp")
(load "fun-get.lisp")
(load "mac-get.lisp")
(load "fun-resolve.lisp")

;Macros that you need to actually do anything with it.
(load "macs/macs.lisp")
(load "macs/mac-fun.lisp")
(load "macs/mac-loops.lisp")
(load "macs/struct.lisp")

;Macros, functions and types regarding:
(load "types/number-types.lisp")
(load "types/pointer-types.lisp")

;Output.(Uses the macros.
(load "to-c.lisp")
