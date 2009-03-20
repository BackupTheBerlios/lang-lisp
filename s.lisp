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

(defmethod c-name ((ts typeset))
  (with-slots (more-specific arg-types) ts
    (print (list (evalm res sum more-specific) arg-types))))

(evalm str c "progn 50.9")

;TODO too much breakage..
(evalm str res "progn-raw
  (do-times (const 3) (i) i)")

(evalm str c "progn-raw
  (let (a 1;)
    (namespace mew
     (let (b 2;)
       (* a b)))" :body-level t :fun-level t)

;Hmm, doesn't work for to C, does work to summary.
(evalm str c "progn-raw (defun name (x (any);) x)")

(evalm str c "progn-raw
  (defun meh (a (int64); b (int64)) (+ (* a b) b a))" :body-level t)
;Note: this one needs meh )defined above
(evalm str c "progn-raw
  (let (a 3 ; b 6) |
    + a (bit-or (+ a 6 1) (meh a b));)" :body-level t :fun-level t)

;TODO decrease dud variables in C output.
(evalm str c "progn-raw
  (let (a 45 ; b 61) |
    progn 5 6 2;
    + a (let ((c (sqr (let ((d a)) (+ b d))))) | * c b;);"
  :body-level t :fun-level t)

;TODO to-c must recognize loop-like macros again.
(evalm str sum "progn-raw
  (let (a 56 ; b 6) |
    while (a) (sqr b);
    |a" :body-level t)

;TODO decrease the ammount of scopes in stuff like this..
(evalm str c "progn-raw
  (let (a 56;) (let ((ref b) 2;) (+ a b)))" :body-level t)


(load "read/read-c.lisp")

(slot-value *state* 'manual-type-coarser)

(in-package #:read-c)

(read-c (lambda()"") "int function(int a, int b);
const double fun2 ();
# define lala 4236373" #'print)

