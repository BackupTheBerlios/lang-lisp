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

(load "other/namespace.lisp")
(load "other/argument.lisp")

(load "read/read.lisp")
(load "read/read-lang.lisp")

(defpackage #:lang
  (:use #:common-lisp #:iterate
	#:generic #:argument #:namespace #:read-lang))
;Function and macro to get stuff from a list like a macro.

(in-package #:lang)
(defvar *state*)

;The code that does the processing to an s-expression with the functions and
;values entirely specified as objects with arguments and return types.
(load "core/fun-base.lisp")

(load "core/type-util.lisp")
(load "core/type-coarser.lisp")
(load "core/typeset.lisp")

(load "core/typeset-named.lisp")
(load "core/fun-get.lisp")
(load "core/mac-get.lisp")
(load "core/fun-resolve.lisp")

(load "core/states.lisp")  ;Extensions of the state that macros/output use.

;Chaining operations.
(load "other/chain.lisp")
(load "other/chain-lang.lisp")

;Macros that you need to actually do anything with it.
(load "macs/macs.lisp")
(load "macs/fun.lisp")
(load "macs/loops.lisp")
(load "macs/struct.lisp")

;Macros, functions and types regarding:
(load "types/any.lisp")
(load "types/eql.lisp")
(load "types/numbers.lisp")
(load "types/ptr.lisp")
(load "types/ref.lisp")

;TODO ;Transformations making things qualify for outputs. (Like to C)

;Output.
(load "convert/conv.lisp")
(load "convert/conv-lisp.lisp")

;Old output method.
(load "convert/to-c.lisp")

;;Auto-documentation stuff.
(require :asdf)
(asdf:operate 'asdf:load-op :documentation-template)

(defun doc-template-document ()
  "Automatically documents langs functions with documentation-template."
  (documentation-template:create-template (find-package '#:lang)
    :subtitle "The main project."
    :target "doc/autodoc/dt-lang.html")
  (documentation-template:create-template (find-package '#:argument)
    :subtitle "Iteration over arguments like macros take."
    :target "doc/autodoc/dt-argument.html")
  (documentation-template:create-template (find-package '#:namespace)
    :subtitle ""
    :target "doc/autodoc/dt-argument.html")
  (documentation-template:create-template (find-package '#:read)
    :subtitle "Iterate over text files, recognizing strings."
    :target "doc/autodoc/dt-read.html"))
;  (documentation-template:create-template (find-package '#:read-c)
;    :subtitle "Reading C headers."
;    :target "doc/autodoc/dt-read-c.html"))

;Doesn't work, probably package troubles?
;(require :tinaa)
;
;(tinaa:document-system :package '#:lang "doc/autodoc/tinaa-lang.html")
