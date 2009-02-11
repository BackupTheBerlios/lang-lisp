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

(defmacro binary-fun-allow-rest (symbols)
  `(progn 
   ,@(loop for s in symbols
       collect
        `(mac-add ,s () () (first second &rest rest)
	   ,(format nil "Allows ~D to have &rest" s)
	   (values (if (null rest)
		       (list ',s first second)
		       (list ',s first (append (list ',s second) rest)))
		   :defer-to-fun)))))
(defmacro inverse-binary-fun-allow-rest (inverse non-inverse)
  `(mac-add ,inverse () () (first second &rest rest)
     (values (if (null rest)
	       (list ',inverse first second)
	       (list ',inverse first
		     (append (list ',non-inverse second) rest)))
	     :defer-to-fun)))

;;Number types large enough for eachother are compatible.
(push (lambda (type compare-type state)
	(when (and (null (cdr type)) (null (cdr compare-type)))
	  (case (car type)
	    (|long-double|
	     (case (car compare-type)
	       ((|long-double| |double| |float|
		 |int64| |int32| |int16| |int8|) t)))
	    (|double|
	     (case (car compare-type)
	       ((|double| |float| |int64| |int32| |int16| |int8|) t)))
	    (|float|  (case (car compare-type)
			((|float| |int32| |int16| |int8|) t)))
	    (|int64|  (case (car compare-type)
			((|int64| |int32| |int16| |int8|) t)))
	    (|int32|  (case (car compare-type)
			((|int32| |int16| |int8|) t)))
	    (|int16|  (case (car compare-type)
			((|int16| |int8|) t)))
	    (|int8|   (case (car compare-type) ((|int8|) t))))))
	(slot-value *state* 'manual-type-generality))

;;Numbers stuff.
(binary-fun-allow-rest (+ * < > <= >=))
(inverse-binary-fun-allow-rest - +)
(inverse-binary-fun-allow-rest / *)

;TODO eventually bignum, fraction, more general number class.
;Make atomic number types.
(add-type '|double| ('atomic-type) :c-name '|double| :size 8)
(add-type '|float|  ('atomic-type) :c-name '|float|  :size 4)
(add-type '|int64|  ('atomic-type) :c-name '|int64|  :size 8)
(add-type '|int32|  ('atomic-type) :c-name '|int32|  :size 4)
(add-type '|int16|  ('atomic-type) :c-name '|int16|  :size 2)
(add-type '|int8|   ('atomic-type) :c-name '|int8|   :size 1)

;TODO what about just int? Equal it to int64 or int32?
;     And unsigned integers?
;TODO why are things turning float? (hmm behavior gone..)
;Note that information _can_ be lost! (int32, int64 -> float)
(let*((order '((|long-double|) (|double|) (|float|)
	       (|int64|) (|int32|) (|int16|) (|int8|)))
      (len (length order)))
  (loop for i from 0 upto (- len 1) do
  (loop for j from 0 upto (- len 1) do
    (dolist (s '(+ - * /))
      (fun-add s `(,(nth i order) ,(nth j order)) ()
	       :out-type (nth (min i j) order) :flags '(:c-binary-fun))))))
;And boolean returning ones.
(let ((numbers
       '((|double|) (|float|) (|int64|) (|int32|) (|int16|) (|int8|))))
  (dolist (n1 numbers)
  (dolist (n2 numbers)
    (dolist (s '(< > <= >=))
      (fun-add s `(,n1 ,n2) ()
	       :out-type '(boolean) :flags '(:c-binary-fun))))))

;Some functions.
(fun-resolve '(|defun| |sqr| |:only-record| |:specify-as-used|
	       (x) (* x x)) '())

;And bitmask/integer-only stuff.
(binary-fun-allow-rest (|\|| |\|\|| & &&))
(inverse-binary-fun-allow-rest % *)

(mac-add ~ () () (&rest rest)
  "Allows usage of multiple things that must all be not true."
  (values (if (null (cdr rest)) `(~ ,@rest) `(~ (& ,@rest))) :defer-to-fun))
(mac-add ! () () (&rest rest)
  "Allows usage of multiple things that must all be not true."
  (values (if (null (cdr rest)) `(! ,@rest) `(! (& ,@rest))) :defer-to-fun))

(let*((integers '((|int64|) (|int32|) (|int16|) (|int8|)))
      (len (length integers)))
  (loop for i from 0 upto (- len 1) do
  (loop for j from 0 upto (- len 1) do
    (dolist (s '(|\|| |\|\|| & && %))
      (fun-add s `(,(nth i integers) ,(nth j integers)) ()
	:out-type (nth (min i j) integers) :flags '(:c-binary-fun)))))
  (dolist (el integers)
    (fun-add '~ `(,el) () :out-type el)
    (fun-add '! `(,el) () :out-type el)))

(fun-resolve '(|progn|
	       ;Synonyms.
	       (|defun| |bit-or|  |:inline| |:only-record| (a b)
		(|\|| a b))
	       (|defun| |bit-and| |:inline| |:only-record| (a b)
		(& a b))
	       (|defun| |bit-not| |:inline| |:only-record| (a b)
		(~ a b))
	       (|defun| |int-or|  |:inline| |:only-record| (a b)
		(|\|\|| a b))
	       (|defun| |int-and| |:inline| |:only-record| (a b)
		(&& a b))
	       (|defun| |int-not| |:inline| |:only-record| (a b)
		(! a b))
	       (|defun| |mod| |:inline| |:only-record| (a b)
		(% a b))
	       ;TODO extension of |%| to |float| and |double|.
	       ;NOTE maybe do via math.h's modf.
	       )
	     nil)
