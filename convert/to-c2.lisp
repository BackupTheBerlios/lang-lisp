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

;File notes:
; In this file lang is converted to C. This version separates a lot of 
; things out of process-code, but may still need improvement on the code 
; itself.

(in-package #:lang)

(defstruct ps ;TODO return-flag, which is passed on up when that is correct.
  body-level end-format-str
  do-auto (tab-depth 0))

(defun process-type (type state ps)
  "Types for which how to write it in C isnt tied to the variable."
  (with-slots (tab-depth do-auto) ps
    (if (not (listp type)) "Any"
      (case (car type)
	((|ptr| |ref|)
	 (format nil "~D*" (process-type (second type) state ps)))
	(t 
	 (let ((typespec (gethash (car type)
				  (get-extension-slot state :types 'types))))
	   (cond
	     ((and* typespec (c-name typespec) (null (cdr type)))
	      (c-name typespec))
;TODO automatic naming and pre-specified naming beyond the primitives.
; (Must also make the actual types as needed)
	     (t
	      (format nil "~D~D"
		      (slot-value  state 'nondescript-type-preface)
		      (size-of type (cdr type) state))))))))))

(defun process-type-var (type var state ps)
  "Processes a variable with a type. Checks if the type needs to use the 
variable name."
  (with-slots (tab-depth do-auto) ps
  (flet ((process-tp (tp)
	   (process-type tp state ps)))
  (case (car type)
    (|function|
     ;Treat as function (Silly that C doesnt give an alternative, it seems.)
     (format nil "~D(*~D)(~{~a~^, ~})"
	     (process-tp (cadr type)) var
	     (loop for tp in (cddr type)
		collect (process-tp tp))))
    (t
     (format nil "~D ~D" (process-tp type) var))))))

(defmethod c-name ((fun fun))
  (with-slots (c-name name) fun
    (if-use c-name name)))

(defun tabbed-body (tab-depth body)
  (format nil (format nil "{~T~~D;~% ~~{~~~DT~~a;~~% ~~} ~~~DT}"
		      tab-depth (- tab-depth 2))
	  (car body) (cdr body)))

(defun tabbed (tab-depth body)
  (format nil (format nil "~~{~~~DT~~a;~~% ~~}" tab-depth) body))

(defun c-gensym (state)
  (gen-c-name state))

(defun strip-last (list) ;TODO put in correct file.
  (loop for el on list
     unless (null(cdr el)) collect (car el)))

(defun carlast (list)
  (car(last list)))

;NOTE will also need to continuously collect the stuff.

(defmethod c-name ((list list))
  (c-name (car list)))


;;TODO horrid code. How about a sort of macro resolving instead?

(defvar *c-process-mac* (make-hash-table))


(defmacro process-aid (state ps &body body)
  (with-gensyms (gstate)
  `(let (prepend (,gstate ,state))
   (with-slots (tab-depth body-level end-format-str) ps
     (flet ((c-return (main)
	      (values main prepend))
	    (process (c &key (tabd 0) body-level end-format-str)
	      (multiple-value-bind (output new-prepend)
		  (process-code c :state ,gstate
		    :ps (make-ps :tab-depth (+ tab-depth tabd)
			  :body-level body-level :end-format-str end-format-str))
         ;Also collects stuff to precede function usage with to allow for
         ; code bodies inside function usage.
		(setf- append prepend new-prepend)
		output)))
       (flet ((process-list (code-list &key (tabd 0)
			     body-level last-body end-format-str)
		(do ((c code-list (cdr c))
		     (out nil 
		       `(,@(list (process (car c) :tabd tabd
				    :body-level (when body-level
						  (if last-body
						      (not(null (cdr c))) t))
				    :end-format-str end-format-str))
			   ,@out)))
		    ((null c) out))))
	 ,@body))))))

;; Maybe add conversion functions to the macros, and use them.
(defun process-code (code &key (state *state*) (ps (make-ps)))
  "Processes code, producing C code."
  
  (unless (listp code)
;    (warn (format nil "Process-code accidentally ate a non-list. ~D" code))
    (setf- list code))
  
  (process-aid state ps
    (case (type-of (car code))
      (fun ;Convert function.
       (argumentize-list (fun &rest args) code
	 (c-return ;Note the c-return here.
	  (cond 
	   ;Identity function needs nothing.
	    ((eql (slot-value fun 'c-name) 'identity)
	     (unless (= (length args) 1)
	       (error "Identity function should only have one argument."))
	     (process (car args)))
  	   ;Binary functions must be written as such.
	    ((in-list (slot-value fun 'flags) :c-binary-fun)
	     (unless (= (length args) 2) ;TODO ditch hooks, when possible.
	       (error "Binary functions have two arguments.\
 (it is a flag of functions.)"))
	     (format nil "(~D ~D ~D)" (process (first args))
		     (c-name fun)
		     (process (second args))))
   	   ;Some things that are known not to need more hooks.
	    ((case (c-name fun) ((* & ~ !) t))
	     (format nil "~D~D"  (c-name fun) (process (car args))))
	    (t ;Regular stuff.
	     (format nil "~D(~{~a~^, ~})"
		     (c-name fun) (process-list args)))))))
      (value ;Convert values encountered.
       (let ((out-type (out-type code)))
	 (c-return
	  (cond
	    ((and* (listp out-type) (eql (car out-type) '|eql|))
	     (format nil "~D" (cadr out-type)))
	    (t
	     (format nil "~D" (from (car code))))))))
      (out   ;Convert various macro results.
       (if-with mac-fun (gethash (slot-value (car code) 'name)
				 *c-process-mac*)
	 (funcall mac-fun state code prepend ps)
	 (error (format nil "Didn't recognize macro. ~D ~D"
			(slot-value (car code) 'name) code)))))))

;;Progn.
(defun c-mac-progn (state code prepend ps)
  "Processes progn."
  (process-aid state ps
    (flet ((dump-prepend ()
	     (let ((out prepend))
	       (setf prepend nil)
	       out)))
      (cond
	(body-level
	 (c-return
	  (tabbed-body tab-depth
	    (loop for c on (cdr code)
	      append
	       (when-with pc
		   (process (car c)
		     :body-level (not(and end-format-str (null(cdr c)))))
		 `(,@(dump-prepend)
		   ,(if (and (null (cdr c)) end-format-str)
			(format nil end-format-str pc)
			pc)))))))
	(t   ;Not at body level, put in gen and
	 (let ((body (process-list (cdr code) :tabd 2
		       :body-level t :last-body t))
	       (gs   (c-gensym state)))
	   (values gs
	       `(,@(strip-last body) ;'nonfunctional' bit of progn.
		 ,@prepend ;Prepend from 'functional' bit of progn.
		 ,(format nil "~D = ~D"
		    (process-type-var (out-type (carlast code)) gs state ps)
		    (carlast body))))))))))

(setf (gethash 'progn *c-process-mac*) #'c-mac-progn)

;;Let.
(defun c-mac-let (state code prepend ps)
  "Processes let."
  (process-aid state ps
    (let ((new-vars
	   (loop for v in (second code)
	      unless (eql (car (out-type (cadr v))) '|eql|) ;Constant.
	      collect
		(format nil "~D= ~D" ;TODO precede variable names to prevent conflict.
		  (process-type-var (out-type (cadr v)) (car v) state ps)
		  (process (cadr v)))))
	  (res (third code)))
      (cond
	(body-level ;At body-level can put vars right here.
	 (c-return
	  (tabbed-body (+ tab-depth 1)
	    `(,@new-vars ,(process res ::body-level body-level)))))
	(t ;Not at body level, pass vars on.
	 (let*((gen (c-gensym state))
	       (varprep prepend)
	       (body (progn
		       (setf prepend nil)
		       (process res :tabd 2 :body-level t
			 :end-format-str
			 (format nil "~D = ~~D"
			   (process-type-var (out-type res) gen state ps))))))
	   (values gen
		   `(,@varprep ,@new-vars
		     ,@prepend ,body))))))))

(setf (gethash 'let *c-process-mac*) #'c-mac-let)

(defun c-mac-while (state code prepend ps)
  (process-aid state ps
    (assert body-level () ;TODO it should be able to do it?
	  "While should not end up in functions.")
    (c-return (format nil (format nil "while(~~D)~%~~~DT~~D" tab-depth)
		(process (second code))
		(process (third code) :body-level t)))))

(setf (gethash 'while *c-process-mac*) #'c-mac-while)

(defun c-mac-defun (state code prepend ps)
  (process-aid state ps
    (c-return (c-name (cadr code)))))

(setf (gethash 'defun *c-process-mac*) #'c-mac-defun)

;;Function stuff (move to own file??)
(defun c-mac-set (state code prepend ps)
  (process-aid state ps
    (c-return (format nil "~D = ~D" (process (second code))
		                    (process (third code))))))

(setf (gethash 'set *c-process-mac*) #'c-mac-set)

(defun process-fun-raw (fun state &key (tab-depth 0)
			(format-str "~D(~{~a~^, ~})"))
  (with-slots (out-type full-code) fun
    (format nil format-str
        ;Here the var is the function name, C is pretty awful in the
	;function pointer thing..
	    (process-type-var out-type (c-name fun) state (make-ps))
	;And the arguments.
	    (loop for arg in (third full-code) collect
		 (process-type-var (second arg) (first arg)
				   state (make-ps))))))

(defun pre-process-fun (fun state)
  "Pre-processes a function. (Notes its existence.)"
  (format nil "~D;~%~D;~%"
    (process-fun-raw fun state)
    (process-fun-raw fun state
		     :format-str "~D_fun(~{~a~^, ~}, void** vars)")))

(defun process-fun (fun &key (state *state*) (tab-depth 0))
  "Processes the code for a function, including .._fun for use as argument."
  (format nil "~D~%~D~2%~D{ return ~D(~{~a~^, ~}); }~2%"
    ;The function.
      (process-fun-raw fun state)
      (process-code (code fun) :state state
	       :ps (make-ps :body-level t :end-format-str "return ~D"
			    :tab-depth tab-depth :body-level t))
    ;For function-as-argument use.
      (process-fun-raw fun state
		       :format-str "~D_fun(~{~a~^, ~}, void** vars)~%")
      (c-name fun)
      (with-slots (full-code) fun
	(loop for arg in (third full-code) collect (first arg)))))

(defun process-all-same-fun (fun state &key write-to)
  (cons (process-fun fun :state state :write-to write-to)
	(loop for f in (more-specific fun)
	   append (process-all-same-fun fun state :write-to write-to))))

(defun process-all-fun (state &key write-to)
  (let (result-list)
    (maphash (lambda (key value)
	       (push (process-all-same-fun value state :write-to write-to)
		     result-list))
	     (funs state))))
