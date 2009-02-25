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

;File notes:
; In this file lang is converted to C. The code is not too good, especially 
; process-code is way too long. Things need to be boiled down.
; Important is that this should not mess with the output itself. Especially
; when doing (out-type code), something done with code before can mess 
; things up.

(in-package #:lang)

(defun process-type (type state &key (tab-depth 0) do-auto)
  "Types for which how to write it in C isnt tied to the variable."
  (flet ((process-tp (tp &key (tab-depth tab-depth) do-auto)
	   (process-type tp state
			 :tab-depth tab-depth :do-auto do-auto)))
    (if (not (listp type)) "Any"
      (case (car type)
	((|ptr| |ref|)
	 (format nil "~D*" (process-tp (second type))))
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

(defun process-type-var (type var state &key (tab-depth 0) do-auto)
  "Processes a variable with a type. Checks if the type needs to use the 
variable name."
  (flet ((process-tp (tp &key (tab-depth tab-depth) do-auto)
	   (process-type tp state :tab-depth tab-depth
				      :do-auto do-auto)))
  (case (car type)
    (|function|
     ;Treat as function (Silly that C doesnt give an alternative, it seems.)
     (format nil "~D(*~D)(~{~a~^, ~})"
	     (process-tp (cadr type)) var
	     (loop for tp in (cddr type)
		collect (process-tp tp))))
    (t
     (format nil "~D ~D"
	     (process-type type state :tab-depth 0 :do-auto t)
	     var)))))

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

;TODO need to make stuff that can't be done functionally in C so that it works.
; Think letting process-code return two values, the current, and one that has
; to be placed before the current. variables will need to be created to feed stuff
; from the arguments that need C-bodies into the functions.

;NOTE will also need to continuously collect the stuff.

(defmethod c-name ((list list))
  (print list)
  (c-name (car list)))

;;TODO horrid code.
;; Maybe add conversion functions to the macros, and use them.
(defun process-code (code &key (state *state*) fun-top
	      (var-precede "") top-level body-level (tab-depth 0) do-auto)
  "Processes code, producing C code."
  (let (body-collected gen-collected)
  (flet ((strip-last (list)
	   (loop for el on list
	      unless (null(cdr el)) collect (car el)))
	 (c-return (main)
	   (values main body-collected gen-collected))
	 (process (c &key (tabd 0) first-fun 
		     body-level (var-precede ""))
	   (multiple-value-bind (output pre-body pre-gen)
	       (process-code c :state state :tab-depth (+ tab-depth tabd)
			:body-level body-level :var-precede var-precede)
 	   ;Also collects stuff to precede function usage with to allow for
	   ; code bodies inside function usage.
	     (setf- append body-collected pre-body)
	     (setf- append gen-collected pre-gen)
	     output)))
  (flet ((process-list (code-list &key (tabd 0)
				       body-level (var-precede ""))
	   (let (out)
	     (dolist (c code-list)
	       (when-with r (process c :tabd tabd :var-precede var-precede
				     :body-level body-level)
		 (push r out)))
	     out)))
    
  (unless (listp code) (setf- list code))
  
  (case (type-of (car code))
    (fun ;Convert function.
     (argumentize-list (fun &rest args) code
       (c-return
	;TODO conversion of types!? (Won't work for references, currently.)
	(cond 
	 ;Identity function needs nothing.
	  ((eql (slot-value fun 'c-name) 'identity)
	   (unless (= (length args) 1)
	     (error "Identity function should only have one argument."))
	   (c-return (process (car args))))
	 ;Binary functions must be written as such.
	  ((in-list (slot-value fun 'flags) :c-binary-fun)
	   (unless (= (length args) 2)
	     (error "Binary functions have two arguments."))
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
       (cond
	 ((and* (listp out-type) (eql (car out-type) '|eql|))
	  (c-return (format nil "~D" (cadr out-type))))
	 (t
	  (c-return (format nil "~D" (from (car code))))))))
    (out   ;Convert various macro results.
     (with-slots (name) (car code)
       ;Some are special and are turned into C control structures, etcetera.
       (case name
	 (flet
	   (c-return (process (cadr code))))
	 (progn
	   (let ((gen  (if body-level "" (gen-c-name state))))
	   (flet ((process-collected ()
		    (loop for b in (reverse body-collected)
			  for g in (reverse gen-collected)
		       append
			 `(,@b
			   ,(format nil "~D= ~D" 
			      (process-type-var (out-type(cadr g))
						(car g) state)
			      (caddr g)))
		       finally (progn (setf body-collected nil)
				      (setf gen-collected nil)))))
	     (cond
	       (fun-top ;Will take the last and make it return there.
		(assert body-level () "Fun-top should imply body-level.")
		(c-return ;TODO make fun-top work properly.
		 (tabbed-body (+ tab-depth 1)
		   (loop for c on (cdr code) 
		      append
			(let ((pc (process (car c) :body-level t)))
			  (when pc
			    `(,@(process-collected)
				,(if (null (cdr c))
				     (format nil "return ~D" pc) pc))))))))
	       (body-level ;At body level(also in fun-top), Take care of
		           ;'body stuff' that came from arguments.
		(tabbed-body (+ tab-depth 1)
		  (loop for c in (cdr code)
		     append
		       (let ((pc (process c :tabd 2 :body-level t)))
			 (when pc
			   `(,@(process-collected) ,pc))))))
	       (t   ;Not at body level, put in gen and
;TODO shouldnt be processing this, only at the receiving side, that way type 
;can be caught and more flexible.
		(let ((body (process-list (cdr code)
					  :body-level t :tabd 2)))
		  (values gen
			  (cons (strip-last body) body-collected)
			  (cons (list gen (car(last code)) (car(last body)))
				gen-collected))))))))
	 (let
	   ;gen avoids name conflicts for code bodies inside function usage.
	   (let*((gen (if body-level "" (gen-c-name state)))
		 (new-vars
		  (loop for v in (second code)
		     unless (eql (car (out-type (cadr v))) '|eql|)
		     collect
		       (format nil "~D= ~D"
		        (process-type-var (out-type (cadr v))
					  (car v) state
			    :tab-depth tab-depth :do-auto do-auto)
			(process (cadr v) :var-precede gen :tabd 2))))
		 (res (third code))
                ;TODO can we stop peeking? (Might be subtil warning!)
		 (is-progn (and* (listp res)
				 (eql (type-of (car res)) 'progn)))
		 (body
		  (if is-progn
		    (process-list (cdr res) :body-level t :tabd 2)
		    (process res :body-level t :tabd 2))))
	     (cond
	       ((= (length new-vars) 0) ;No variables, no work.
		(if is-progn
		  (c-return (tabbed tab-depth body))
		  (c-return body)))
	       (body-level ;At body-level can put vars right here.
		(c-return(tabbed-body (+ tab-depth 1)
			   `(,@new-vars
			     ,@(if is-progn body (list body))))))
	       (t ;Not at body level, pass vars on.
		(let ((gen (gen-c-name state)))
		  (values
		   gen
		   (cons (if is-progn `(,@new-vars ,@(strip-last body))
			              new-vars)
			 body-collected)
		   (cons
		    (list gen
			  (if is-progn (car(last res)) res)
			  (if is-progn (car (last body)) body))
		    gen-collected)))))))
	 (while
	   (assert body-level () ;TODO it should be able to do it.
		   "While should not end up in functions.")
	   (c-return (format nil (format nil "while(~~D)~%~~~DT~~D" tab-depth)
			     (process (second code))
			     (process (third code) :body-level t))))
	 (if ;Also does cond, when, unless. (They must convert to this.)
	   (c-return
	    (argumentize-list (cond when-t when-f) (cdr code)
	      (if body-level
		;At body level, use the if.
		  (format nil "if(~D)~%~Delse~D"
		    (process cond)
		    (tabbed-body tab-depth
				 (list(process when-t :body-level t)))
		    (tabbed-body tab-depth
				 (list(process when-f :body-level t))))
		;Otherwise, use the '?'
		  (format nil "(~D ? ~D : ~D)" (process cond)
			  (process when-t) (process when-f))))))
;TODO funcall...
	 (defun ;Pass on the name, no variable data needed.
	   (c-return (c-name (cadr code))))
;	 (lambda ;Pass on the name, and precede with a 
;	         ;bunch of stuff to feed data.
;	   (values (c-name (code (car code)))))
	    
;	 (funcall ;Variable functions called the same as normal ones in C.
;	  (with-slots (name flags) (third code)
;	    (cond
;	      ((in-list flags :lambda)
;	       (format nil "~D(~{~a~^, ~}, )"
;		       name (process-list (cddr code))
;		       
;	      (t
;	       (format nil "~D(~{~a~^, ~}, NULL)"
;		 (process (second code)) (process-list (cddr code))))
;	 (return ;TODO tricky, (progn (return bla))
;   	             could trigger it at wrong time!
;	   (unless body-level (error "May not return from outside body level"))
;	   (format nil "return(~D)" (process (second code))))
	 (set
	  (c-return (format nil "~D = ~D" (process (second code))
			    (process (third code)))))
	 (void-end
	  (c-return nil))
	 (t
	   (error (format nil "Didn't recognize macro. ~D" code)))))))))))


(defun process-fun-raw (fun state &key (tab-depth 0) do-auto
			(format-str "~D(~{~a~^, ~})"))
  (with-slots (out-type full-code) fun
    (format nil format-str
        ;Here the var is the function name, C is pretty awful in the
	;function pointer thing..
	    (process-type-var out-type (c-name fun)
			      state :tab-depth tab-depth)
	;And the arguments.
	    (loop for arg in (third full-code) collect
		 (process-type-var (second arg) (first arg) state
			   :tab-depth tab-depth :do-auto do-auto)))))

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
      (process-code (code fun) :state state :fun-top t :body-level t
		    :tab-depth tab-depth :body-level t)
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
