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

(require :iterate)

;Handy macros.
(load "other/generic.lisp")

(load "other/documented.lisp")
(load "other/namespace.lisp")
(load "other/argument.lisp")

(load "read/read.lisp")
(load "read/read-lang.lisp")

(load "other/chain.lisp")

(defpackage #:lang ;;TODO chop package up.
  (:use #:common-lisp #:iterate #:documented
	#:chain
	#:generic #:argument #:namespace #:read-lang))
;Function and macro to get stuff from a list like a macro.

(in-package #:lang)

(load "other/chain-lang.lisp")

(load "core/state.lisp")

(load "core/fun-get.lisp")
(load "core/mac-get.lisp")

(load "core/local.lisp")
(load "core/resolve.lisp")

;Base macros.
(load "macs/macs.lisp")
(load "macs/fun.lisp")

;Types.
(load "types/numbers.lisp")

;Transformation.
(load "transform/code-funarg-debody.lisp") ;Needed for conversion to C.

;Output.
(load "convert/conv.lisp")
(load "convert/lisp.lisp")
(load "convert/c.lisp") ;Uses function-argument debodying.