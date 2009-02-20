;
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
  (:use #:common-lisp #:generic #:reader))

(in-package #:lang)

(defun initial-convertable (code)
  "Gives types for inside lang to types of common lisp"
  (cond
    ((integerp code) '(|int64|))
    ((numberp code)  '(|double|))))

;;State of the function inference
(defclass fun-state ()
  (
 ;Functions. (Getting them individually in fun-get.lisp)
   (funs :accessor funs :initform (make-hash-table))
 ;Manual override for generality of functions.
  ;TODO overrides need to be named.
   (manual-type-coarser :initform nil :type list)
   (manual-type-coarser-names :initform nil :type list)
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
   
 ;Extensions by macros.
   (extensions :initform nil :initarg extensions :type list)
   ))

(defun fun-state-manual-type-coarser (state name)
  "Gets a manual coarser function."
  (with-slots (manual-type-coarser manual-type-coarser-names) state
    (loop for fun in manual-type-coarser
	  for n in manual-type-coarser-names
       when (eql n name)
       return fun)))

(defun (setf fun-state-manual-type-coarser) (function state name)
  "Sets a manual coarser function."
  (with-slots (manual-type-coarser manual-type-coarser-names) state
    (unless (loop for fun in manual-type-coarser
	          for n in manual-type-coarser-names
	       when (eql n name)
	       return (setf fun function))
      (push function manual-type-coarser)
      (push name manual-type-coarser-names))))

(defun get-extension (state extension-name)
  (getf (slot-value state 'extensions) extension-name))
(defun (setf get-extension) (to state extension-name)
  (setf (getf (slot-value state 'extensions) extension-name ) to))

(defmacro get-extension-slot (state extension-name slot)
  `(slot-value (get-extension ,state ,extension-name) ,slot))

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
