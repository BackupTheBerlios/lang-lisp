
(in-package #:lang)


(defmethod c-name ((fun fun))
  (if-use (if-use (get-name fun :c) (get-name fun :usual))
	  (slot-value fun 'name)))

(defmethod c-name ((tp base-type))
  (if-use (get-name tp :c) (get-name tp :usual)))

(defmethod c-name ((list list))
  (car list))

;;Note that converting to C requires that it is transformed with
;; code-funarg-debody.

(defclass conv-c-state (conv-state)
  ((state :initarg :state)
   (gen-format :initfrom "GEN~D" :initarg :gen-format :type string)
   (gen-cnt :initform 0 :initarg :gen-cnt :type integer)
   (had-names :documentation "Names encountered at this scope.\
 Used to avoid namespace collisions."
    :initform nil :initarg :had-names))
  (:documentation "State for C conversion."))

(defun conv-c-state-new-scope (conv-state)
  (with-slots (gen-format gen-cnt state) conv-state
    (make-instance 'conv-c-state :state state
		   :gen-format gen-format :gen-cnt gen-cnt)))
;
;(defun c-gensym (state)
;  (with-slots (gen-format gen-cnt) state
;    (intern(format nil gen-format gen-cnt))))
;
;(defun add-gensym (state name)
;  (intern(format nil "~D_~D" (c-gensym state) name)))
;
;(defun conv-c-name (name)
;  "Makes the name fit in C."
;  (when (symbolp name) (setf name (symbol-name name)))
;  (iter (for ch in-vect name)
;	(case ch
;	  (#\- (setf ch #\_))))
;  (intern name))

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
	      (conv-c-name (symbol-name(c-name typespec))
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
  (iter (for c in body)
	(collect (conv c))))

(make-conv *conv-c-macs* 'let (vars body)
  (with-slots (had-names) conv-state
    (append (iter
	      (for v in vars)
	      (collect (format nil "~D ~D = ~D"
			 (c-conv-type (out-type (second v)) conv-state)
			 (first v) (conv (second v))))
	    (conv body)))))

(make-conv *conv-c-macs* 'while (cond body)
  `(,(format nil "while (~D)" (conv cond))
    (,(format nil (conv-code body (conv-c-state-new-scope conv-state))))))

(defun c-conv-fun (code conv-state)
  "Function useage conversion to C."
  (let* ((fun (car code))
	 (c-name (c-name fun)))
    (cond
      ((in-list (slot-value fun 'flags) :c-binary-fun)
       (if (third code)
	 (format nil "(~D ~D ~D)" (second code) c-name (third code))
	 (format nil "(~D ~D)" c-name (second code))))
      (t
       (format nil "~D(~{~a;, ~})" c-name
	       (iter (for a in (cdr code))
		     (collect (conv-code a conv-state))))))))

(defun c-conv-value (code conv-state)
  "Value useage conversion to C."
  (from (car code)))

(defvar *c-conv-state*
  (make-conv-state #'c-conv-fun #'c-conv-value *conv-c-macs*))
;  "Conversion state for converting to lisp.")

(defun c-body-ize (list &optional (tabdepth 1))
  (format nil "~{~a}"
    (iter (for el in list)
	  (collect (if (listp el)
		     (c-body-ize el (+ tabdepth 1))
		     (format nil (format nil "~~~DT~~D;" tabdepth)
			     el))))))

;;TODO convert code for C.
(defun c-conv-function (fun &optional (conv-state *c-conv-state*))
  "Converts a single function."
  (with-slots (args-code code) fun
    (format nil "~D ~D (~{~a;, ~})~%{~T~%~D}"
	    (c-conv-type-var (out-type fun) (get-name fun :c) conv-state)
	    (iter (for a in args-code)
		  (collect (if (listp a)
			     (c-conv-type-var (cadr a) (car a) conv-state)
			     (format nil "Any ~D" a))))
	    (c-body-ize (conv-code code conv-state)))))


(defun ,(get-name fun :lisp)
	 (,@       ,(conv-code code conv-state))))

(defun c-conv-all-functions (state &optional (conv-state *c-conv-state*))
  "Converts all functions in state, outputs them in list."
  (with-slots (funs) state
    (iter (maphash (lambda (key fun)
		     (collect (c-conv-function fun conv-state)))
		   funs)
	  (finish))))
