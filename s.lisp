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

(load "read/read-c.lisp")

(in-package #:lang)

(print 'a)

(evalm str c "progn 50.9")

(evalm str c "progn-raw (defun name ((x (any))) x)")

(evalm str c "progn-raw
  (do-times (const 3) (i) i)")

(evalm str c "progn-raw
  (let (a 1;)
    (namespace mew
     (let (b 2;)
       (* a b)))" :body-level t)

;TODO why does it protest at first? Why going through a raw typeset?
;  And in process-code for that matter, that should be (nearly)stateless.
(evalm str c "progn-raw
  (defun meh (a (int64); b (int64)) (+ (* a b) b a))" :body-level t)

;Note: this one needs meh
(evalm str c "progn-raw
  (let (a 3 ; b 6) |
    + a (bit-or (+ a 6 1) (meh a b));)" :body-level t)

(evalm str c "progn-raw
  (let (a 45 ; b 61) |
    progn 5 6 2;
    + a (let ((c (sqr (let ((d a)) (+ b d))))) | * c b;);" :body-level t)

(evalm str c "progn-raw
  (let (a 56 ; b 6) |
    while (a) (sqr b);
    |a" :body-level t)

(evalm str c "progn-raw
  (let (a 56;) (let ((ref b) 2;) (+ a b)))" :body-level t)

(type-list-coarser '((any)) '((any)))

(type-list-eql '((any)) '((any)))


(in-package #:read-c)

(read-c (lambda()"") "int function(int a, int b);
const double fun2 ();
#define lala 4236373" #'print)

