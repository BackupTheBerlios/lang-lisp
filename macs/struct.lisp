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

(defclass types-state ()
  ((types :initarg :types :initform (make-hash-table))
   
   (pointer-type-size :initarg :pointer-type-size
		      :initform 16 :type integer)
   (arbitrary-type-size :initarg :arbitrary-type-size
			:initform 16 :type integer))
  (:documentation "State of the types holds some sizes and the types\
 defined upto latest point."))

;Add extension.
(unless (get-extension *state* :types)
  (setf (get-extension *state* :types) (make-instance 'types-state)))

;If it does not have the extension already, extend it.

;;TODO creation of stuff.

(defclass base-type ()
  ((names  :initarg :names :initform nil)
   (name  :initarg :name :initform nil :accessor name)))

(defclass atomic-type (base-type)
  ((size  :initarg :size :initform nil :accessor size)
   (size-fun :initarg :size-fun :initform nil :accessor size-fun)))

(defclass struct-type (base-type)
  ;code is: name, args, elements
  ((code  :initarg :code :initform nil :accessor code)))

(defmacro add-type (name (type &optional (state '*state*)) &rest rest)
  "Adds an type."
  (with-gensyms (the-name)
    `(let ((,the-name ,name))
      (setf (gethash ,the-name (get-extension-slot ,state :types 'types))
	    (make-instance ,type :name ,the-name ,@rest)))))

(defgeneric size-of (type with-el state)
  (:documentation "Returns the size of whatever the representation of the\
 type. WARNING dont confuse cl:type-of! I made no such function."))

(defmethod size-of (type with-el (state fun-state)) ;Defer to types-state.
  (size-of type with-el (get-extension state :types)))

(defmethod size-of ((type symbol) (with-el list) (state types-state))
  (if-with typed (assoc type with-el)
    ;Got that type specified, use it.
      (size-of (cadr typed) with-el state)
    ;Not specified, give cost of arbitrary type.
      (slot-value state 'arbitrary-type-size)))

(defmethod size-of ((type list) (with-el list) (state types-state))
  (size-of (gethash (car type) (slot-value state 'types))
	   with-el state))

(defmethod size-of :around ((type atomic-type) (with-el list)
			    (state types-state))
  (with-slots (size size-fun) type
    (cond
      (size-fun (funcall size-fun type with-el))
      (size     size)
      (t        (error "Atomic types need manual size determination.")))))

(defmethod size-of :around ((type struct-type) (with-el list)
			    (state types-state))
  (shift-upto type nil with-el state))

(defmethod size-of (type (with-el list) (state types-state))
  (error (format nil "missed-type ~D ~D" type with-el)))

(defun shift-upto (struct upto-element with-el state)
  "Calculate shift to the given element."
  (declare (type struct-type struct) (type list with-el))
  (argumentize-list (name (&rest args) &rest el) (cdr (code struct))
    (declare (ignorable name args))
    (let (found-el)
      (values (loop for el in el
		 when (eql (car el) upto-element)
		 do (setf found-el el)
		 until (eql (car el) upto-element)
		 sum (size-of (cadr el) with-el state))
	      found-el))))

(defun get-struct-args (type structspec)
  "Gets an assoc list with type by struct argument name."
  (loop for fill-arg in (cdr type)
        for arg      in (caddr (code structspec))
     collect (list arg fill-arg)))

;TODO one with non-constant slot name? how about (not(any))?
(rawmac-add get-slot () ((eql (symbol)) (any)) (slot-name obj)
  "Macro returning the code and type getting the slot that is wanted."
  (with-slots (types) (get-extension state :types)
  (let*((obj-res (fun-resolve obj type-of :state state))
	(type    (out-type obj-res)))
    (cond
    ;Don't know the type, need to learn it at run-time.
      ((symbolp type)
       (fun-resolve `(get-slot-fun ',slot-name ,obj) type-of :state state))
      ((gethash (car type) types)
       (let ((typespec (gethash (car type) types)))
	 (case (type-of typespec)
	   (atomic-type
	    (error "Atomic-types don't have slots."))
	 ;Is a struct, start counting upto our desired element.
	   (struct-type
	    (let ((with-el (get-struct-args type typespec)))
	      (multiple-value-bind (struct-shift found-el)
		  (shift-upto typespec slot-name with-el state)
		(unless found-el
		  (error (format nil "get-slot: ~D is not a slot of ~D"
				 slot-name type)))
	      ;Return the result.
		`(,(make-instance 'out :name 'get-slot
		   ;Fill the type arguments with those we have.
		     :type(type-fill (cadr(assoc slot-name 
						 (cddr(code typespec))))
				     with-el))
		   ,struct-shift ,slot-name ,obj-res))))
	   (t
	    (error "What the hell is this typespec?")))))
      (t
       (error (format nil "get-slot: Structures need to exist for my use.
Asked for slot ~D in struct ~D." slot-name (car type))))))))

(rawmac-add size-of () (anything) (obj)
  "Size of an type. (Without extra pointer indirection.)"
  (declare (ignorable anything))
  (let*((type (out-type(fun-resolve obj type-of :state state)))
	(typespec
	 (when (listp type) ;Look for the structure specification.
	   (gethash (car type)
		    (get-extension-slot state :types 'types)))))
    (make-instance 'out :name 'size-of
      :type `(eql ,(size-of type
			  ;When structure, need to get arguments.
			    (case (type-of typespec)
			      (struct-type (get-struct-args type typespec)))
			    state)))))

(rawmac-add struct (:code code) () (name)
  "Struct creation macro."
  (setf (gethash name (get-extension-slot state :types 'types))
	(make-instance 'struct-type :name name :code code))
  (list(make-instance 'out :name 'struct :type '(:structspec)
		      :code code)))
