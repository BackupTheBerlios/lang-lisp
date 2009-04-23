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

(defmethod c-name ((fun fun))
  (if-use (if-use (get-name fun :c) (get-name fun :usual))
	  (slot-value fun 'name)))

(defmethod c-name ((tp base-type))
  (if-use (if-use (get-name tp :c) (get-name tp :usual))
	  (slot-value tp 'name)))

;;Note that converting to C requires that it is transformed with
;; code-funarg-debody.

(defclass conv-c-state (conv-state)
  ((state :initarg :state)
   (precidence :initarg :precidence :initform '(- + / % *))
   (in    :initarg :in :initform nil)
   (return :initform t :initarg :return)))

(defun conv-c-state-changed (conv-state &key return in)
  "Returns a changed copy."
  (when (eql return :same)
    (setf return (slot-value conv-state 'return)))
  (when (eql in :same)
    (setf in (slot-value conv-state 'in)))
  (with-slots (conv-fun conv-value conv-macs state) conv-state
    (make-instance 'conv-c-state :state state :return return :in in
		   :fun conv-fun :value conv-value :macs conv-macs)))

(defun conv-c-name (name)
  "Makes the name fit in C."
  (when (symbolp name) (setf name (symbol-name name)))
  (iter (for ch in-vector name)
	(case ch
	  (#\- (setf ch #\_) )))
  (intern name))

(defvar *conv-c-macs* (make-hash-table))

(defun c-conv-type (type conv-state)
  "Types for which how to write it in C isnt tied to the variable."
  (with-slots (state) conv-state
    (if (not (listp type)) "Any"
      (case (car type)
	((|ptr| |ref|)
	 (format nil "~D*" (conv-type (second type) conv-state)))
	(t 
	 (let ((typespec (gethash (car type)
			   (get-extension-slot state :types 'types))))
	   (cond
	     ((and typespec (c-name typespec) (null (cdr type)))
	      (conv-c-name (symbol-name(c-name typespec))))
;TODO automatic naming and pre-specified naming beyond the primitives.
; (Must also make the actual types as needed)
	     (t
	      (format nil "~D~D"
		(slot-value state 'nondescript-type-preface)
		(size-of type (cdr type) state))))))))))

(defun c-conv-type-var (type var conv-state)
  "Processes a variable with a type. Checks if the type needs to use the 
variable name."
  (flet ((conv-tp (tp)
	   (conv-type tp conv-state)))
    (case (car type)
      (|function|
     ;Treat as function (Silly that C doesnt give an alternative, it seems.)
       (format nil "~D(*~D)(~{~a~^, ~})"
	 (conv-tp (cadr type)) var
	 (iter (for tp in (cddr type))
	       (collect (conv-tp tp)))))
      (t
       (format nil "~D ~D" (conv-tp type) var)))))

;Rule here that all convs must make list.
(make-conv *conv-c-macs* 'progn (&rest body)
  "Progn"
  (flet ((list-ize (x) (if (listp x) x (list x))))
    (iter (for c on body)
	  (appending (list-ize
	    (if (null (cdr c))
	      (conv (car c) (conv-c-state-changed conv-state :return :same))
	      (conv (car c)
		    (conv-c-state-changed conv-state))))))))

(make-conv *conv-c-macs* 'let (vars body)
  "Let"
  (with-slots (had-names) conv-state
    (append (iter
	      (for v in vars)
	      (collect
		  (format nil "~D ~D = ~D"
		    (c-conv-type (out-type (second v)) conv-state) (first v)
		    (conv (second v) (conv-c-state-changed conv-state)))))
	    (conv body))))

(make-conv *conv-c-macs* 'while (cond body)
  "While"
  `(;(:no-semicolon
     ,(format nil "while (~D)" (conv cond));)
    ,(conv-code body (conv-c-state-changed conv-state))))

(make-conv *conv-c-macs* 'void-end ()
  "" "")

(defun c-conv-fun (code conv-state)
  "Function useage conversion to C."
  (with-slots (return in precidence) conv-state
    (let* ((return-str (if return "return " ""))
	   (fun (car code))
	   (c-name (c-name fun)))
      (flet ((conv (code &key in)
	       (conv-code code (conv-c-state-changed conv-state :in in))))
	(cond
	  ((in-list (slot-value fun 'flags) :c-binary-fun)
	   (let ((with-hooks (iter (for fun in precidence)
				   (cond ((eql fun c-name)
					  (return nil))
					 ((eql fun in)
					  (return t))))))
	     (if (third code)
		 (format nil (if with-hooks "~D(~D ~D ~D)" "~D~D ~D ~D")
			 return-str (conv (second code) :in c-name)
		     c-name (conv (third code)))
		 (format nil (if with-hooks "~D(~D ~D)" "~D~D ~D")
			 return-str c-name (conv (second code) :in c-name)))))
	  (t
	   (format nil "~D~D(~{~a~^, ~})" return-str c-name
		   (iter (for a in (cdr code))
			 (collect (conv a :in :fun))))))))))

(defun c-conv-value (code conv-state)
  "Value useage conversion to C."
  (if (slot-value conv-state 'return)
    (format nil "return ~D" (from (car code)))
    (from (car code))))

(setf *c-conv-state*
  (make-instance 'conv-c-state
    :fun #'c-conv-fun :value #'c-conv-value :macs *conv-c-macs*
    :state *state* :return t))
;  "Conversion state for converting to lisp.")

(defun c-body-ize (list &key (tabdepth 1))
  "Interprets nested lists as c-bodies."
  (format nil (format nil "~~~DT{~~%~~{~~~DT~~a~%~~}~~~DT}"
		      (- tabdepth 1) tabdepth (- tabdepth 1))
    (iter (for el in list)
	  (collect
	      (cond
		((listp el)
		 (case (car el)
		   (:no-semicolon
		    (cadr el))
		   (t
		    (c-body-ize el :tabdepth (+ tabdepth 1)))))
		((= tabdepth 0)
		 (format nil "~D;" el))
		((> tabdepth 0)
		 (format nil "~D;" el))
		(t
		 (error "Lower-then-zero tabdepth.")))))))

;;TODO convert code for C.
(defun c-conv-function (fun &optional (conv-state *c-conv-state*))
  "Converts a single function."
  (with-slots (args-code code) fun
    (format nil "~D ~D (~{~a~^, ~})~%{~T~%~D}"
	    (c-conv-type-var (out-type fun) (get-name fun :c) conv-state)
	    (iter (for a in args-code)
		  (collect (if (listp a)
			     (c-conv-type-var (cadr a) (car a) conv-state)
			     (format nil "Any ~D" a))))
	    (c-body-ize (conv-code code conv-state)))))

(defun c-conv-all-functions (state &optional (conv-state *c-conv-state*))
  "Converts all functions in state, outputs them in list."
  (with-slots (funs) state
    (iter (maphash (lambda (key fun)
		     (collect (c-conv-function fun conv-state)))
		   funs)
	  (finish))))
