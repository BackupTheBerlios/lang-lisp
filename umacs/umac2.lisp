(defpackage #:umac
  (:use #:common-lisp #:generic)
  (:export))

(in-package #:umac)

(defvar *umac-macs* (make-hash-table))

(setf *umac-macs* (make-hash-table))

(defun def-umac-fun (name fun &optional (umac-macs *umac-macs*))
  (setf (gethash name umac-macs) fun))

(defun add-var (have-vars add-var)
  "Adds a variable to have-vars."
  (flet ((delist (x) (if (listp x) (car x) x)))
    (unless (or (null add-var)
		(loop for v in have-vars
		   when (eql (delist v) (delist add-var)) return t))
      `(,add-var ,@have-vars))))

(defmacro def-umac (name (&key (inc 0)
			       (umac-macs *umac-macs*)
			       (vars (gensym)) (end-name (gensym))
			       (cnt (gensym)) (cnt-num (gensym)))
		 (&rest arguments) &body body)
  (with-gensyms (args res new-vars)
    `(def-umac-fun ',name
       (lambda (,cnt ,cnt-num ,args)
	 (argumentize-list (,@arguments) ,args
	   (let (,vars ,end-name)
	     (flet ((set-end-name (to-name)
		      (setf ,end-name to-name))
		    (umac- (code)
		      "Process lower stuff."
		      (multiple-value-bind (,res ,new-vars)
			  (argumentize-list (name &rest args) code
			  (umac-fun ,umac-macs ,cnt (+ ,cnt-num ,inc)
				    name args))
			(dolist (v ,new-vars)
			  (setf- add-var ,vars v))
			,res)))
	       (values ;(progn ,@body) ,
		(if ,end-name
		     `(let ((,,end-name ,(+ ,cnt-num ,inc)))
			 ,(progn ,@body))
		     (progn ,@body))
		,vars)))))
       ,umac-macs)))

(def-umac body () (&rest body)
  "Just runs the body."
  `(progn ,@body))

(def-umac var () (var-name value)
  "Creates variable."
  (values nil `(,var-name ,value)))

(def-umac var-list () (&rest var-list)
  "Creates multiple variables."
  (values nil var-list))

(def-umac series-no-inc ()
		 (series-name &rest series)
  "Does umacs expression in series."
  (set-end-name series-name)
  `(progn ,@(loop for el in series collect
		 `(unless (umac-is-end ,series-name)
		    ,(umac- el)))))

(def-umac series (:inc 1)
		 (series-name &rest series)
  "Does umacs expression in series."
  (set-end-name series-name)
  `(progn ,@(loop for el in series collect
		 `(unless (umac-is-end ,series-name)
		    ,(umac- el)))))

(def-umac loop (:inc 1) (loop-name &rest series)
  "Loops a series until (end ',loop-name) is called."
  (set-end-name loop-name)
  `(do () ((umac-is-end ,loop-name) nil)
     ,(umac- `(series-no-inc ,loop-name ,@series))))

(def-umac repeat (:inc 1) (count &rest series)
  "Repeat something some number of times. If count a list, it is the 
counting index, upto-value and then the name you can umac-end-to with."
  (argumentize-list (i upto repeat-name)
      (if (listp count) count (list (gensym) count))
    (set-end-name repeat-name)
    (umac- `(loop ,repeat-name
	       (sum ,counter 1)
	       (stop-when (< ,counter ,count))
	       (series-no-inc ,repeat-name ,@series)))))

(def-umac stop-when (:cnt-num cnt-num) (conditions)
  `(when (and ,@conditions)
     (umac-end-to ,(- cnt-num 1))))
(def-umac stop-unless (:cnt-num cnt-num) (conditions)
  `(unless (and ,@conditions)
     (umac-end-to ,(- cnt-num 1))))

;;Collecting/summing/etc.
(def-umac collect () (to &rest collected)
  "Collects to variable."
  (values `(setf- append ,to (list ,@collected)) `((,to nil))))

(def-umac append () (to &rest appended)
  "Appends to variable."
  (values `(setf- append ,to ,@appended) `((,to nil))))

(def-umac sum () (to &rest summed)
  "Sums to variable."
  (values `(setf- + ,to ,@summed) `((,to 0))))

(def-umac funcall () (to fun &rest args)
  "Function-call changing."
  (values `(setf to (funcall fun to args)) `(,to)))

(def-umac operation () (to operation &rest args)
  "Some arbitrary operation applied to something."
  (values `(setf- ,operation ,to ,@args) `(,to)))

(defmacro umac ((&optional (umac-macs *umac-macs*) (cnt (gensym)))
		&rest series)
  ""
  (multiple-value-bind (result variables)
      (umac-fun umac-macs cnt 0 'series `(top ,@series))
    `(let ((,cnt -1) ,@variables)
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
