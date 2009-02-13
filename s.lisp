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
(load "play-test-util.lisp")

(in-package #:lang)

(with-slots (more-specific)
    (get-symbol 'do-times (slot-value *state* 'macs) *state*)
  (loop for el in more-specific
     collect el))

(typeset-get (get-symbol 'do-times (slot-value *state* 'macs) *state*)
	     '((|eql| 4)))

(slot-value *state* 'manual-type-generality)

(type-coarser '(|eql| (|integer| n)) '(|eql| 2))

(process-code (fun-resolve '(let1 (sum 0)
			(do-times (const 3) (i) (set sum (+ i sum)))
			sum) '()) :body-level t)

(summary(fun-resolve '(const 0) '()))

(summary(fun-resolve '(* b a) '((a (|int32|)) (b (|int32|)))))
(summary(fun-resolve '(defun meh ((x (|int32|))) (* x x)) '()))

(summary(fun-resolve '(meh a) '((a (|int32|)))))

(process-code(fun-resolve
	      '(progn(+ a (|bit-or| a b))) '((a (|int32|)) (b (|int32|))))
	     :body-level t :fun-top t)


(summary(fun-resolve '(|sqr| a) '((a (|int64|)))))

(setf (gethash '|sqr| (funs *state*)) nil)

(fun-resolve '(defun |sqr| |:only-record| |:specify-as-used|
	       (x) (* x x)) '())

(summary (gethash '|sqr| (funs *state*)))

(process-code(fun-resolve
	 '(progn(+ a (let ((c (|sqr| (let ((d a)) (+ b d))))) (* c b))))
	 '((a (|int32|)) (b (|int32|)))) :body-level t)

(summary(fun-resolve
	 '(while (a) (|sqr| (+ a (let ((c b)) c))))
	 '((a (|int16|)) (b (|int16|))))); :body-level t)

(print 'a)

(summary(named-typeset-get '|sqr| '((|double|)) (funs *state*)
			   :state *state*) :more-on-fun t)

(type-list-coarser '((|int32|)) '((|int32|)))

(type-list-eql-like '((|int32|)) '((|int32|)))

(summary (cadr(fun-resolve '(defun |sqr| |:specify-as-used| ((X (|double|))) (* X X))
			   '())))
