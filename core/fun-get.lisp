;;
;;  Copyright (C) 2009-04-03 Jasper den Ouden.
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
   (args-code :initarg :args-code :initform nil)
   
 ;Flags are: :check-eql*, :inline..
   (flags     :initarg :flags :initform nil)
   
   (doc-str :initarg :doc-str :initform "" :type string)

   (names :initarg :names :initform nil :type list))
  (:documentation "Holds the information of functions in Lang.
In resolved code, it should be first of a list, (possibly)followed by\
 arguments"))

;The reason this function is more complicated then just a wrapper round 
;named-typeset-get is that it also handles functions that are to be 
;specified as used. TODO move that to resolve?
(defun fun-get (name arg-types
		     &key funs (state *state*))
  "Gets the function."
  (unless funs (setf funs (slot-value state 'funs)))
  (named-typeset-get name arg-types funs :state state))

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

;;Conversion, getting and setting.
(defun conv-get (from-tp to-tp &key (state *state*))
  "Gets the conversion"
  (typeset-get (slot-value state 'conversion) `(,from-tp ,to-tp)
	       :state state :no-conversion t))

(defun (setf conv-get) (to from-tp to-tp &key (state *state*))
  "Sets the conversion"
  (setf (typeset-get (slot-value state 'conversion) `(,from-tp ,to-tp)
		     :state state :no-conversion t)
	to))

(defmacro conv-add (name from-tp to-tp (&key (state '*state*)) &rest rest)
  "Makes a conversion function."
  (with-gensyms (the-args state-var)
    `(let*((,state-var ,state)
	   (,the-args `(,,from-tp ,,to-tp)) (,state-var ,state))
       (setf (conv-get ,the-args :state ,state-var)
	     (make-instance 'fun
		:name (namespace-symbol ,name ,state-var)
		:arg-types ,the-args :out-type (cadr ,the-args)
		,@rest)))))
