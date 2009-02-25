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

(defun initial-convertable (code)
  "Gives types for inside lang to types of common lisp"
  (cond
    ((integerp code) '(|int64|))
    ((numberp code)  '(|double|))))

;;State of the function inference
(defclass fun-state (namespace-state)
  (
 ;Functions. (Getting them individually in fun-get.lisp)
   (funs :accessor funs :initform (make-hash-table))
 ;Manual override for generality of functions.
  ;TODO overrides need to be named.
   (manual-type-coarser :initform nil :type list)
   (manual-type-coarser-names :initform nil :type list)
   
 ;Conversion-funs. Take two arguments, from, to.
   (conversion :initform (make-hash-table))

 ;(raw)Macros. (Getting these individually in mac-get.lisp)
   (macs :accessor macs :initform (make-hash-table))
   
 ;Converts types from stuff like common lisps integers.
   (convert-type :initarg :convert-type
		 :initform #'initial-convertable)
      
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
    (if-use (loop for i from 0
	           for n in manual-type-coarser-names
	       when (eql n name)
	       return (setf (nth i manual-type-coarser) function))
      (progn
	(push function manual-type-coarser)
	(push name manual-type-coarser-names)))))

(defun get-extension (state extension-name)
  (getf (slot-value state 'extensions) extension-name))
(defun (setf get-extension) (to state extension-name)
  (setf (getf (slot-value state 'extensions) extension-name ) to))

(defmacro get-extension-slot (state extension-name slot)
  `(slot-value (get-extension ,state ,extension-name) ,slot))

