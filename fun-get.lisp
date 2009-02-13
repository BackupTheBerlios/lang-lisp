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

(defclass fun (typeset-named)
;  A function with types for result and arguments. contains a list of more
;   specific functions.
  ((out-type  :initarg :out-type  :initform nil :accessor out-type)
   (code      :initarg :code      :initform nil :accessor code)
   (full-code :initarg :full-code :initform nil :accessor full-code)
   
   ;TODO doesn't belong?
   ;Function that outputs a string doing the function in C.
   (c-name :initarg :c-name
	   :initform nil :type (or null string))
   (c-str  :initarg :c-str  :accessor c-str
	   :initform nil :type (or null string))
   
   ;Flags are: :check-eql*, :inline..
   (flags     :initarg :flags :initform nil)
   
   (doc-str :initarg :doc-str :initform "" :type string)))

(defun type-list-eql-like (tl1 tl2 &key (state *state*))
  (and (type-list-coarser tl1 tl2 :state state)
       (type-list-coarser tl2 tl1 :state state)))

(defun fun-get (name arg-types
		     &key funs (state *state*))
  "Gets the function."
  (unless funs (setf funs (slot-value state 'funs)))
  (when-with got (named-typeset-get name arg-types funs :state state)
    (with-slots (flags full-code) got
      (cond
	((and* (in-list (slot-value got 'flags) :specify-as-used)
	       (not (type-list-eql-like arg-types (arg-types got) 
					:state state)))
;TODO how does this interact with namespace??
;TODO problem: order of operation matters..
;   Make more specific first, then more general, it makes both,
;   reverse, it only makes more general.
       ;Make the more specified function.
	 (let ((want (loop for el on (cdr full-code)
			when (listp (car el))
			return el)))
	   (fun-resolve
	     `(|defun| ,(cadr full-code) |:specify-as-used|
	       ,(loop for a  in arg-types ;Switch to current types.
		   for fa in (car want)
		   collect `(,(if (listp fa) (car fa) fa) ,a))
	       ,@(cdr want))
	     (typelist-get-var (arg-types got) arg-types
			       :state state :do-types nil)
	    :state *state*))
       ;It should be in there now, get it.
	 (fun-get name arg-types :funs funs :state state))
	(t
	 got)))))

(defun (setf fun-get) (to name arg-types &key funs (state *state*))
  "Adds a function.(Does not replace old function unless exact!)
Warning: arg types of the added function must be correct!"
  (declare (type fun to))
  (unless funs (setf funs (slot-value state 'funs)))
  (setf (named-typeset-get name arg-types funs :state state) to))

(defmacro fun-add (name arg-types (&key (state '*state*)) &rest rest)
  "Makes a function with (setf fun-get), shaves off make-instance."
  (with-gensyms (the-args the-name state-var)
    `(let*((,state-var ,state)
	   (,the-args ,arg-types) (,state-var ,state)
	   (,the-name ,name))
       (setf (fun-get ,the-name ,the-args :state ,state-var)
	     (make-instance 'fun
		:name (namespace-symbol ,the-name ,state-var)
		:arg-types ,the-args
		,@rest)))))

