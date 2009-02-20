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

;Stuff to play with. 
(load "other/play-test-util.lisp")

(in-package #:lang)

(with-slots (more-specific)
    (get-symbol 'do-times (slot-value *state* 'macs) *state*)
  (loop for el in more-specific
     collect el))

(typeset-get (get-symbol 'do-times (slot-value *state* 'macs) *state*)
	     '((|integer|)))

(type-coarser '(|any|) '(|int64|))

(eval-str (:to-c) "progn 50.9")

(eval-str (:to-c :body-level t) "progn-raw
  (do-times (const 3) (i) i)")

(eval-str (:to-c :body-level t :fun-top t) "progn-raw
  (let (a 1 ; b 4) |
    * a b;)")

(eval-str (:to-c :body-level t) "progn-raw
  (defun meh (a (int64); b (int64)) (+ (* a b) b a))")

(eval-str (:to-c :body-level t) "progn-raw
  (let (a 3 ; b 6) |
    + a (bit-or (+ a 6 1) (meh a b));)")

;(with-open-file (stream "miauw" :if-exists :supersede :direction :output)
;  (format stream
(eval-str (:to-c :body-level t) "progn-raw
  (let (a 45 ; b 61) |
    progn-raw 5 6 2 ;
    + a (let ((c (sqr (let ((d a)) (+ b d))))) | * c b;);")

(eval-str (:to-c :body-level t) "progn-raw
  (let (a 56 ; b 6) |
    while (a) (sqr b);
    |a")

(eval-str (:tokenize) "")

(eval-str (:to-c :body-level t) "progn-raw
  (let (a 56;) (let (b (ref a);) (+ a b)))")

(eval-str (:to-c :body-level t :vars '((|a| (|ptr| (|int64|)))))
  "progn-raw (val a)")

(print 'a)
(setf (get-symbol '|ref| (funs *state*) *state*) nil)


(typelist-get-var '(any) '((ptr (int))))
