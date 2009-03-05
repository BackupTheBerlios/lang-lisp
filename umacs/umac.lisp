(defpackage #:umac
  (:use #:common-lisp #:generic)
  (:export))

(in-package #:umac)

(defvar *umac-macs* (make-hash-table))

(defun def-umac-fun (name fun &optional (umac-macs *umac-macs*))
  (setf (gethash name umac-macs) fun))

(defun add-var (have-vars add-var)
  "Adds a variable to have-vars."
  (flet ((delist (x) (if (listp x) (car x) x)))
    (unless (or (null add-var)
		(loop for v in have-vars
		   when (eql (delist v) (delist add-var)) return t))
      `(,add-var ,@have-vars))))

(defmacro def-umac (name (&key (inc 0) pass-first
			  may-end (any-may-end (gensym))
			  (umac-macs *umac-macs*)
			  (vars (gensym)) (cnt (gensym)) (cnt-num (gensym)))
		 (&rest arguments) &body body)
  (with-gensyms (args res end-name out)
    `(def-umac-fun ',name
       (lambda (,cnt ,cnt-num ,args)
	 (let (,vars (,any-may-end ,may-end) ,end-name)
	   (flet ((add-var (var)
		    (setf- add-var ,vars var))
		  (set-end-name (to-name)
		    (setf ,end-name to-name))
		  (umac- (code)
		    "Process lower stuff."
		    (multiple-value-bind (,res new-vars may-end)
			(argumentize-list (name &rest args) code
			  (umac-fun ,umac-macs ,cnt (+ ,cnt-num ,inc)
				    name args))
		      (dolist (v new-vars)
			(setf- add-var ,vars v))
		      (when may-end (setf ,any-may-end t))
		      (values ,res may-end ,pass-first))))
	     (let ((,out (argumentize-list (,@arguments) ,args
			   ,@body)))
	       (values (if ,end-name
			   `(let ((,,end-name ,(+ ,cnt-num ,inc)))
			      ,,out)
			   ,out)
		       ,vars ,any-may-end)))))
	 ,umac-macs)))

(defmacro umac ((&optional (umac-macs *umac-macs*) (cnt (gensym)))
		&rest series)
  ""
  (multiple-value-bind (result variables)
      (umac-fun umac-macs cnt 0 'series `(top ,@series))
    `'(let ((,cnt -1) ,@variables)
       (flet ((umac-is-end (at-cnt)
		(unless (< ,cnt 0)
		  (<= ,cnt at-cnt)))
	      (umac-end-to (to-cnt)
		(setf ,cnt to-cnt)))
	 ,result)
       ,(flet ((get-var (name) 
		 (loop for v in variables
		    when (eql (if (listp v) (car v) v) name)
		    return (if (listp v) (car v) v))))
	  (let ((val-names '(val-1 val-2 val-3 val-4 val-5 val-6 val-7)))
	    (cond
	      ((loop for v in val-names
		  when (get-var v) return t)
	       `(values ,(get-var 'ret)
			,@(loop for v in val-names
			     collect (get-var v))))
	      ((get-var 'ret)
	       (get-var 'ret))
	      (t
	       nil)))))))

(defun umac-fun (umac-macs cnt cnt-num name args)
  (when-with fun (gethash name umac-macs)
    (funcall fun cnt cnt-num args)))
