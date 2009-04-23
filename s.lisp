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

;Lang stuff and base lang libraries.
(load "loader.lisp")

(in-package #:lang)

(conv-c-state-changed *c-conv-state* :in 'miauw)

;TODO too much breakage..
(evalm str res "progn-raw
  (do-times 4 (i) i)")

(print 'a)

(evalm res lisp (code-funarg-debody (evalm str res "progn (+ 2 3 4")))

(evalm str sum "defun meh (x y) (+ x (* 2 y))"))

(slot-value (fun-get *local* '|meh|) 'variants)

(evalm str res "+ 3 (meh 4 5)") ;TODO make satisfying-subset.

(load "read/read-c.lisp")

(in-package #:read-c)

(with-open-file (stream "/usr/include/stdint.h")
  (read-c (lambda () (read-line stream nil nil)) " " #'print))

(with-open-file (stream "/usr/include/stdint.h")
  (reader "aa define miauw" (lambda () (read-line stream nil nil)) #'is-whitespace
    `(("define" (lambda (getstr str)
		  (print str) ""))
      ("" ,(lambda (getstr str)
	     "")))))

(let ((list (list nil "define miauw 2")))
  (read-c (lambda () (print(car(setf- cdr list))))
	  "int function(int a, int b);
const double fun2 ();
# define lala 4236373" #'print))

(stream-from-str 

(print 'a)

(require :asdf-install)

(asdf-install:install :linedit)

(require :linedit)

(use-package :linedit)

(do-symbols (var (find-package :linedit))
  (print var) t))
