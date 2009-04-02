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

;This comes from using get-symbol on the macs slot of fun-state.
(defclass mac-set (typeset)
  ((typeset-arg-cnt :initform 0 :initarg :typeset-arg-cnt :type integer)
   (item :initform nil :initarg :item)))

;Macros are just functions that operate on code.
(defclass mac (typeset-named)
  ((doc-str :initarg :doc-str :initform "")
   (fun  :initarg :fun :initform (lambda (code)(cdr code))
	 :accessor fun)))

;Rawmacros get all the state information, including variables, as such they
;can create variables. However, avoid it and use rawmacros from normal 
;macros where-ever possible.
(defclass rawmac (typeset-named)
  ((doc-str :initarg :doc-str :initform "")
   (fun  :initarg :fun
	 :initform (lambda (code type-of &key funs macs rawmacs)
		     (declare (ignorable funs macs rawmacs type-of))
		     (values (cdr code) :again))
	 :accessor fun)))

(defun mac-get (name arg-types &key macs (state *state*))
  "Gets a macro."
  (unless macs (setf macs (slot-value state 'macs)))
  (when-with got (get-symbol name macs state)
    (with-slots (typeset-arg-cnt item) got
      (case typeset-arg-cnt
	(0 (assert (= (length arg-types) 0) ()
		   "(langs fault) Why am i getting argument types?")
	   item) ;No argument parts.
	(t (typeset-get got arg-types :state state))))))

(defun (setf mac-get) (to name arg-types
	       &key macs (state *state*) replace)
  "Sets a macro."
  (unless macs (setf macs (slot-value state 'macs)))
  (when to
    (let ((got (get-symbol name macs state)))
      (cond
       ;Asked to replace or nothing in the hash table here yet.
	((or replace (null got))
	 (setf (get-symbol name macs state)
	       (let ((len (length arg-types)))
		 (make-instance 'mac-set :typeset-arg-cnt len
		   :item (when (= len 0) to)
		   :more-specific (unless (= len 0) (list to))))))
	((not(= (length arg-types) (slot-value got 'typeset-arg-cnt)))
	 (error "Type selection of macros may only use a single number of \
arguments. Use :replace to remove previous macros with this name."))
	(t
	 (with-slots (typeset-arg-cnt item) got
	   (case typeset-arg-cnt
	     (0 ;No argument parts. (replacing)
	      (assert (= (length arg-types) 0) ()
		      "(langs fault) Why am i getting argument types?")
	      (setf item to))
	     (t ;Got arguments.
	      (setf (typeset-get got arg-types :state state)
		    to)))))))))

(defmacro mac-add-direct (name arg-types mactp
			  (&key (state *state*) replace (i t)) &rest rest)
  "Makes a function with (setf mac-get), shaves off make-instance."
  (with-gensyms (the-args the-state the-name set-to)
    `(let*((,the-args ,arg-types) (,the-state ,state)
	   (,the-name (namespace-symbol ,name ,the-state))
	   (,set-to
	    (make-instance ',mactp :name ,the-name :arg-types ,the-args
			   ,@rest)))
       (setf (mac-get ,the-name ,the-args
		      :state ,the-state :replace ,replace)
	     ,set-to)
       (when ,i
	 (setf (mac-get (intern(string-downcase(symbol-name ,the-name)))
			,the-args :state ,the-state :replace ,replace)
	       ,set-to)))))

(defmacro let-list (list &rest body)
  `(eval (append (list 'let ,list)
		 (list ,@body))))

(defmacro let-assoc ((&rest varlist) assoc-list &body body)
  `(let (,@(loop for var in varlist
	      collect `(,var (cadr (assoc ',var ,assoc-list)))))
     ,@body))

(defmacro mac-add (name
    (&key (state '*state*)
	  (code (gensym)) (actual-types (gensym)) replace (i t))
    (&rest arg-types) (&rest arguments) &body body)
  "Adds a non-raw macro."
  `(mac-add-direct ',name (list ,@(loop for el in arg-types collect `',el))
		   mac (:state ,state :replace ,replace :i ,i)
     :doc-str ,(if (stringp(car body))
		  (let ((str (car body))) (setf- cdr body) str)
		  "")
     :fun
     (lambda (,code &optional ,actual-types)
       (declare (ignorable ,code ,actual-types))
       (argumentize-list (,@arguments) (cdr ,code)
	 ,@(if (= 0 (length arg-types))
	     body
	     (let ((got-vars (typelist-list-var arg-types)))
	       (if (= 0 (length got-vars))
		 body
		 `((let-assoc (,@got-vars)
		       (typelist-get-var '(,@arg-types) ,actual-types
					 :state ,state)
		     ,@body)))))))))

(defmacro rawmac-add (name 
    (&key (state '*state*) (state-var 'state) (type-of 'type-of)
	  (code (gensym)) (actual-types (gensym)) replace (i t))
    (&rest arg-types) (&rest arguments) &body body)
  "Adds a raw macro."
  `(mac-add-direct ',name (list ,@(loop for el in arg-types collect `',el))
		   rawmac (:state ,state :replace ,replace :i ,i)
     :doc-str ,(if (stringp(car body))
		  (let ((str (car body))) (setf- cdr body) str)
		  "")
     :fun
     (lambda (,code ,type-of &optional ,actual-types (,state-var ,state))
       (declare (ignorable ,code ,type-of ,actual-types ,state-var))
       (argumentize-list (,@arguments) (cdr ,code)
	 ,@(if (= 0 (length arg-types))
	     body
	     (let ((got-vars (typelist-list-var arg-types)))
	       (if (= 0 (length got-vars))
		 body
		 `((let-assoc (,@got-vars)
		       (typelist-get-var '(,@arg-types) ,actual-types
					 :state ,state-var)
		     (declare (ignorable ,@got-vars))
		     ,@body)))))))))

