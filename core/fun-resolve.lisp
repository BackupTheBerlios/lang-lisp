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

(defmacro with-fun-resolve (&body body)
  "Adds function resolve in flet for brevity. Requires state argument in
 same name argument."
  `(flet ((resolve (code type-of &key defer-to-fun)
	    (fun-resolve code type-of :state state
	       :defer-to-fun defer-to-fun)))
     ,@body))

;Simple values like from constants and variables go in these.
(defclass value ()
  ((out-type :initarg :type :initform nil :accessor out-type)
   (from :initarg :from :initform nil :accessor from))
  (:documentation "Holds the information of values in Lang."))

;Macro results go in these. (TODO code -> from?)
(defclass out ()
  ((name  :initarg :name :initform nil)
   (code      :initarg :code :initform nil)
   (full-code :initarg :full-code :initform nil)
   (out-type  :initarg :type :initform nil :accessor out-type)
   (data      :initarg :data :initform nil))
  (:documentation "Holds information of final-macro output of Lang.
Usually as first element in list, with arguments following."))

(defmethod out-type ((list list))
  (argumentize-list (obj &rest args) list
    (case (type-of obj)
      (fun
       (if (in-list (slot-value obj 'flags) :chase-args)
	  (type-fill (slot-value obj 'out-type)
	    (typelist-get-var (slot-value obj 'arg-types)
			      (iter (for arg in args)
				    (collect (out-type arg)))))
	  (out-type obj)))
      (t
       (out-type obj)))))

(defun value-resolve (code type-of &key state)
  "Resolves the type of a value."
  (multiple-value-bind (type from)
      (cond
	((symbolp code) ;Get symbol, scan the namespaces.
	 (get-symbol-assoc code (list type-of) state))
	(t
	 (values (funcall (slot-value state 'convert-type) code)
		 code)))
    (make-instance 'value :from from :type type)))

(defun fun-check (fun arg-types)
  "Check if function with these argument types needs further specifying."
  (with-slots (flags full-code) fun
    (cond
      ((and* (in-list flags :specify-as-used)
	     (not (type-list-eql arg-types (arg-types got) 
				 :state state)))
;TODO how does this interact with namespace??
;TODO problem: order of operation matters..
;   Make more specific first, then more general, it makes both,
;   reverse, it only makes more general.
       ;Make the more specified function.
       (let ((want (iter (for el on (cdr full-code))
			 (when (listp (car el))
			   (return el)))))
	 (fun-resolve ;Resolve a new function.
	  `(|defun| ,(cadr full-code) |:specify-as-used|
	     ,(iter (for a  in arg-types) ;Switch to current types.
		    (for fa in (car want))
		    (collect `(,(if (listp fa) (car fa) fa) ,a)))
	     ,@(cdr want))
	  (typelist-get-var (arg-types got) arg-types
			    :state state :do-types nil)
	  :state *state*))
     ;It should be in there now, get it.
       (fun-get name arg-types :funs funs :state state))
      ((in-list flags :inline) ;If inline, yank it here.
      ;TODO namespaces can break it, prevent that from happening.
       (resolve (let ((want (iter (for el on full-code)
				  (when (listp (car el))
				    (return el)))))
		  `(|let| (,@(iter (for a  in arguments)
				   (for fa in (car want))
				   (collect `(,(if (listp fa)
						   (car fa) fa)
					       ,a))))
			  ,@(cdr want)))
		type-of))
      (t
       fun))))

(defun fun-resolve (code type-of &key (state *state*) defer-to-fun)
  "Resolves the functions of the code. (Function inference instead of type\
 inference.)
Returns the tree with the names replaced with function structs, and\
 variables paired with their types everywhere."
  (with-fun-resolve
  (cond
  ;Nothing.
    ((null code)
     (make-instance 'out :name 'void-end :type '(|void|)))
  ;Already done for some reason.
    ((case (type-of code) ((fun value out) t))
     code)
  ;An value/variable.
    ((not (listp code))
     (value-resolve code type-of :state state))
  ;Already done for some reason, but written as list.
    ((case (type-of (car code)) ((fun value out) t))
     code)
  ;Its a (raw)macro.
    ((and (get-symbol (car code) (slot-value state 'macs) state)
	  (not defer-to-fun))
     (let ((macset (get-symbol (car code) (slot-value state 'macs) state)))
       (assert macset () "Could not find macro of this name." (car code))
       (with-slots (typeset-arg-cnt item) macset
	 (let*(arg-types
	       (mac
		(case typeset-arg-cnt
		  (0 ;Nothing to check, its just a macro.
		   item)
		  (t ;Select which macro.
		   (setf arg-types;Results of resolve otherwise discarded.
			 (iter (repeat typeset-arg-cnt)
			       (for a in (cdr code))
			    (collect (out-type(resolve a type-of)))))
		   (typeset-get macset arg-types :state state)))))
	   (multiple-value-bind (result validity)
	       (case (type-of mac) ;Use the function inside.
		 (null
		  (error "Could not find macro with the given types."))
	       ;Actual and those of macro compared inside macro evaluation.
		 (mac
		  (funcall (fun mac) code arg-types))
		 (rawmac
		  (funcall (fun mac) code type-of arg-types state))
		 (t
		  (error "(langs fault)What is this type of macro?")))
	     (case validity ;Take messages from the macro.
	       (:defer-to-fun ;Do the result with only functions.
		              ; (For functions on the same name.
		(resolve result type-of :defer-to-fun t))
	       (:discard ;Discard result an defer to function instead.
		(resolve code type-of :defer-to-fun t))
	       (:again ;Resolve again.(Only useful for rawmac.)
		(resolve result type-of))
	       (:is-done
		result)
	       (t (case (type-of mac)
		    (mac (resolve result type-of))
		    (rawmac result)))))))))
  ;It should be a function.
    (t  ;Get results of arguments.
     (let*((arguments (iter (for el in (cdr code))
			    (collect (resolve el type-of))))
	 ;Get function with the types.
	   (types     (iter (for arg in arguments)
			    (collect (out-type arg))))
	   (fun (fun-get (car code) types :state state)))
       (when (null fun) ;Didn't get a function.
	 (error "Couldn't find function/macro matching: ~D ~D"
		(car code) types))
       ;See if have to create/inline function.
       (setf- fun-check fun types)
       ;Return result.
       (cons fun arguments))))))


;Not at all tested.
(defun count-var-dependencies (res &optional so-far)
  "Determines what variables res depends on, and how many times."
  (cond
    ((listp res) ;Should be tail recursive.
     (count-var-dependencies
      (cdr res)
      (count-var-dependencies (car res) so-far)))
    ((eql (type-of res) 'value)
     (if-with got (assoc (from res) so-far)
       (progn (setf- + (cadr got) 1)
	      so-far)
       (cons (list (from res) 1) so-far)))))