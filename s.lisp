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

(setf (get-symbol 'do-times (slot-value *state* 'macs) *state*) nil)

(print 'a)

(evalm str c "progn-raw
  (let (a 2;)
    (namespace mew
     (let (b 2;)
       (* a b)))")

;Hmm, doesn't work for to C, does work to summary.
(evalm str c "progn-raw (defun name (x (any);) x)")

(evalm str res "progn-raw
  (defun meh (a (int64); b (int64)) (+ (* a b) b a))" :body-level t)
;Note: this one needs meh )defined above
(evalm str c "progn-raw
  (let (a 3 ; b 6) |
    + a (bit-or (+ a 6 1) (meh a b));)")

(evalm str c
       "progn (let ((a (+ 1 (+ 1 2))) (b 2)) (meh (* (+ 2 3) a) b))")

;TODO decrease dud variables in C output.

;TODO it lost the first let, setting c,
; Hmm, it is when more then one element in progn.
(evalm str lisp "progn-raw
  (let (a 45 ; b 61)
    (+ (let ((q 3)) (+ q 2)) 6)
    (progn 5 6 2)
    (+ a (let ((c (* (let ((d a)) (+ b d)) 2))) 1 (* c b))))")

(macroexpand-1 '
(argumentize-list ((a b) c) '((1 2) 3)
		 (list a b c)))

;TODO bug
(evalm str lisp "progn | meh 2 5;
  while (2) (meh 1 2) 1;")

;TODO decrease the ammount of scopes in stuff like this..
(evalm str c "progn-raw
  (let (a 56;) (let ((ref b) 2;) (+ a b)))")

(slot-value *state* 'manual-type-coarser)

(process-fun (fun-get '|meh| '((|int64|) (|int64|))))

(evalm str res "progn-raw
  (defun main ((argc (int32)) (argv (ptr(ptr(int8)))))
    0)")
(process-fun (fun-get '|main| '((|int32|) (|ptr|(|ptr|(|int8|))))))

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
