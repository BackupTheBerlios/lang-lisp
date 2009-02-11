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
;;Tests fun.lisp

(load "../generic/generic.lisp")
(load "reader.lisp")

(load "argument-positioner.lisp")

(load "fun-base.lisp")
(load "typeset.lisp")
(load "typeset-named.lisp")
(load "fun-get.lisp")
(load "mac-get.lisp")
(load "fun-resolve.lisp")

(load "macs.lisp")
(load "mac-fun.lisp")

(load "struct.lisp")

(load "base-types.lisp")

(load "to-c.lisp")

(in-package #:lang)

(cons 1 '(2 3))

(loop for el in '(1 2 3 4)
     collect el
     finally (return 1))

(string-lessp 'b 'a)

;Try adding functions
(setf *state* (make-instance 'state-with-types))
(setf *types* (slot-value *state* 'types))

(type-more-general '(int) '(any))

(slot-value (gethash '+ (slot-value *state* 'funs)) 'more-specific)

(typeset-get-list '((int (int)) (int))
		  (list(gethash '+ (slot-value *state* 'funs))))

(push 'a (car (list(gethash '+ (slot-value *state* 'funs)))))

(fun-add '+ '((int (int)) (int)) () :out-type 'a)
(fun-add '+ '((int (a)) (int)) () :out-type 'blah)

(fun-add '+ '((int) blah) () :out-type 'blah)
(fun-add '+ '((any) (any)) () :out-type 'bleh)
(fun-add '+ '((int) (int))() :out-type '(int))

(fun-add '* '(bleh blah)  () :out-type 'bleh)
(fun-add '* '((int) (int))() :out-type '(int))

(funs *state*)
(slot-value (gethash 'progn (macs *state*))
	    'item)

(list-summary (fun-resolve '(+ a b) '((a (int)) (b (int)))))

(list-summary (get-symbol '+ (funs *state*) *state*))

(print-summary(get-symbol '* (funs *state*) *state*))
(print(namespace-symbol '* *state*))

(with-slots (namespaces) *state*  (setf namespaces '()))

(defvar *typeof* '((a (int)) (b (int)) (c f) (d g)))

(defun list-summary (input &key more-on-fun more-on-mac)
"Makes the print more reasonable, but not quite enough."
  (flet ((summary (sub-input)
	   (list-summary sub-input
		    :more-on-fun more-on-fun ::more-on-mac more-on-mac)))
  (case (type-of input)
    (cons
     (loop for el in input collect
	  (summary el)))
    (fun
     (with-slots (name arg-types out-type more-specific) input
       `(:fun ,name ,arg-types ,out-type
	      ,@(cond
		 (more-on-fun
		  `(:more-specific ,(summary more-specific)))))))
    (out
     (with-slots (name out-type code) input
       (append (list :out name out-type)
	       (when more-on-mac `(:code ,(summary code))))))
    (value
     (with-slots (out-type from) input
       `(:val ,(summary from) ,out-type)))
    (t
     input))))
(defmacro print-summary (input &rest stuff)
  `(print(list-summary ,input ,@stuff)))

(list-summary(gethash 'brainz_+ (funs *state*)) :more-on-fun t)
(print-summary (gethash 'sqr (funs *state*)))

(list-summary(fun-resolve '(+ a b) *typeof*))

;(lambda ((x (int)) (y (int))) (+ b y (* x a

(list-summary(fun-resolve '(+ a b)
			   '((a (int (int))) (b (int)))) :more-on-fun t); *state* :body-level t)

(list-summary(fun-resolve '(fun-of '+) *typeof*))

(out-type(value-resolve 'a *typeof* :state *state*))

(process-code(fun-resolve '(progn a (+ a (* b (progn a)))) *typeof*) *state*
	     :body-level t)
(process-code(fun-resolve '(progn a
			    (+ a (flet ((q ((x (int))) (* x x))) a (+ a (q b)))))
			  *typeof*)
	     *state* :body-level t)

(slot-value *state* 'namespaces)

(fun-get '* '((int) (int)))

;'(let ((q (+ a b)) (r b))
;		    (+ q r)) *typeof*) *state*)

(format nil (format nil "{~~{~%~~~DT~~a;~~% ~~}}~~%" 2)
	'(a b c))

(print *state*)

(list-summary(fun-resolve '(defun sqr (x) (* x x)) *typeof*))
(list-summary(fun-resolve '(defun sqr ((x (int))) (* x x))
				*typeof*))

(list-summary(fun-resolve '(defun identity ((x (int))) b a x) *typeof*))

(process-code(fun-resolve '(progn (+ a (let ((x a)) (+ x b)))) *typeof*)
	     *state* :body-level t)

(list-summary(fun-resolve '(fun-of '*) *typeof*))

(with-slots (arg-types more-specific)
(typeset-get '('(any))
	     (list (slot-value (get-symbol 'fun-of (macs *state*) *state*) 'item)))
  more-specific)

(list-summary(fun-get '+ '((int) a)))

(list-summary(fun-resolve
	       '(struct array ((var n (int)) el) (arr (ptr)))
	       *typeof*))

(type-more-general '(any) '(eql (quote any))))
;(eql (quote any)) 


(list-summary(fun-resolve
	       '(progn
		 (struct pair (type)
		  (first type) (second type))
		 (struct alien (head-type arms-type legs-type)
		  (head head-type) (arms (pair arms-type))
		  (legs (pair legs-type))))
	       *typeof*))

(list-summary(fun-resolve
	       '(+ c (size-of b))
	       `((b miauw) (a (alien (int) arb (int))) (c (int))))
	       :more-on-mac t)

(list-summary (fun-resolve
	   '(get-slot 'legs a)
	   `((a (alien (int) arb (int))))) :more-on-mac t)

(list-summary (fun-resolve '(quote a) *typeof*))

(list-summary (fun-resolve
	   '(get-slot 'second (get-slot 'legs a))
	   `((a (alien (int) arb (int))))) :more-on-mac t)

(list-summary (code (fun-get 'identity '((int)))))
(process-fun (fun-get 'identity '((int)))  *state*)
(get-symbol 'fun-of (macs *state*) *state*)

;TODO test this once 'if' is implemented.
(list-summary(fun-resolve
	      '(defun mah ((x (int)) (y (int)))
		(+ y (* x a))) *typeof*))

(setf meh (list(list)))

(setf (typeset-get '((int) a) meh)
      (make-instance 'named-typeset :arg-types '(a a) :name 's))


(print (slot-value (typeset-get '(a a) meh) 'more-specific))

(slot-value (typeset-get '(a a) meh) 'name)

