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
(defpackage #:lang
  (:use #:common-lisp #:generic))

(in-package #:lang)

(defun not-found-expression-message (&optional (str-fun #'warn))
  (lambda (code arguments)
    (funcall str-fun
      (format nil "[not-found-expression]Can't find function/macro for the\
 symbol ~D and argument types ~D." (car code)
        (loop for a in arguments
	   collect (out-type (if (listp a) (car a) a)))))))

(defun inconsistent-out-types-message (&optional (str-fun #'warn))
  (lambda (got want)
    (funcall str-fun
      (format nil "[inconsistent-out-types]\
Out-types of existing functions can currently not be changed.\
 If it is not a function, and out-types are not applicable\
 (like macros) out-type should be nil.(making macros via usual way will
 have it that way already.)"))))

(defun initial-convertable (code)
  "Gives types for inside lang to types of common lisp"
  `(|eql| ,code))

;;State of the function inference
(defclass fun-state ()
  (;Functions. (Getting them individually in fun-get.lisp)
   (funs :accessor funs :initform (make-hash-table))
   ;Manual override for generality of functions.
   (manual-type-generality :initarg :manual-type-generality
    :initform (list(lambda (type compare-type state)
		     (eql (car type) 'any))))
   ;(raw)Macros. (Getting these individually in mac-get.lisp)
   (macs :accessor macs :initform (make-hash-table))
   
   ;Converts types from stuff like common lisps integers.
   (convert-type :initarg :convert-type
		 :initform #'initial-convertable)
      
   ;For generating symbols.
   (gen-str :initform 'gen)
   (gen-cnt :initform 0)
   
   (namespaces :initform nil)
 ;The one new stuff should go to. (Use of it is in flet and such.)
   (write-namespace :initform nil)
   
;Information on how to give errors. (Well, not all.)
  
  ;When can not find any (matching) macro/function.
   (not-found-expression :initarg :not-found-expression
			 :initform (not-found-expression-message))
  ;When out-types of something you are changing doesn't match what you \
  ; declared before.
;   (inconsistent-out-types :initarg :not-found-expression
	;		   :initform (inconsistent-out-types-message))))
   ))

(defun add-namespace (namespace symbol)
  (intern (format nil "~D-~D" namespace symbol)))

;;Some base functions and stuff.
(defun namespace-symbol (symbol state)
  "Adds current namespace to symbol."
  (with-slots (write-namespace) state
    (if (null write-namespace)
	symbol (add-namespace write-namespace symbol))))

(defun get-symbol (symbol from state)
  (let (got)
    (loop for namespace in (slot-value state 'namespaces)
       until (setf got (gethash (add-namespace namespace symbol) from)))
    (if-use got (gethash symbol from))))

(defun (setf get-symbol) (to symbol from state)
  (setf (gethash (namespace-symbol symbol state) from) to))
(defun gen-c-name (state)
"Generates a name symbol."
;(NOTE if this doesnt play well with hash table, do something about it.)
  (with-slots (namespaces gen-str gen-cnt) state
    (setf- + gen-cnt 1)
    (intern(format nil "~D~D~D" (if-use (car namespaces) "")
		                gen-str gen-cnt))))
