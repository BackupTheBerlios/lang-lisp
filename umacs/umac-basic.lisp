(in-package #:umac)

(def-umac body () (&rest body)
  "Just runs the body."
  `(progn ,@body))

(def-umac do () (thing)
  `(progn ,thing))

(def-umac var () (var-name value)
  "Creates variable."
  (add-var `(,var-name ,value))
  nil)

(def-umac var-list () (&rest var-list)
  "Creates multiple variables."
  (dolist (v var-list)
    (add-var v))
  nil)

(defmacro progn-like (&body body)
  `(do () (nil nil) ,@body))

(defmacro series-stuff (series-name (end-cond last-op) series)
  (with-gensyms (pass-after)
  `(progn
    (set-end-name ,series-name)
    (let (,pass-after)
      `(,@(if (not ,end-cond)
	      '(progn-like)
	      `(do () (,,end-cond nil)))
	 ,@(loop for el on ,series
	      if (not(null(cdr el)))
	      append (multiple-value-bind (res may-end pass-first)
			 (umac- (car el))
		       (let ((out
			      `(,res
				,@(when may-end
				    `((unless (mac-is-end ,,series-name)
					(return nil)))))))
			 (cond (pass-first (setf- append ,pass-after out)
					   nil)
			       (t          out))))
	      else
	      collect (umac- (car el)))
	 ,@,pass-after
	 ,,last-op)))))

(def-umac series (:inc 1) ;Whether may end determined by umac-
		 (series-name &rest series)
  "Does umacs expression in series."
  (let ((series-name (if (symbolp (car series)) (car series) (gensym))))
    (set-end-name series-name)
    (series-stuff series-name (nil '(return nil))
		  (if (symbolp (car series)) (cdr series) series))))

;;Path control.
(def-umac if () (cond if-t if-f)
  `(if ,cond ,(umac- if-t) ,(umac if-f)))

(def-umac when () (cond &rest series)
  ` (when ,cond ,(if (cdr series)
		     (umac- `(series ,nil ,@series))
		     (umac- (car series)))))

(def-umac unless () (cond &rest series)
  `(unless ,cond ,(if (cdr series)
		      (umac- `(series ,nil ,@series))
		      (umac- (car series)))))

(def-umac cond () (&rest clauses)
  `(cond ,@(loop for c in clauses
	      collect `(,(car c) ,(mac- (cadr c))))))

(def-umac case () (&rest clauses)
  `(case ,@(loop for c in clauses
	      collect `(,(car c) ,(mac- (cadr c))))))

(def-umac while () (&rest conds)
  (umac- `(unless ,(if (cdr conds) `(and ,@conds) (car conds))
	    (finish))))

(def-umac until () (&rest conds)
  (umac- `(when ,(if (cdr conds) `(and ,@conds) (car conds))
	    (finish))))

(def-umac finish (:cnt cnt :cnt-num cnt-num :any-may-end any-may-end)
    (&optional to)
  "Finishes, goes to level provided, or one down if that indicated."
  (when to
    (when (< to cnt-num) (setf any-may-end t))
    (setf cnt to))
  `(return nil))

(def-umac return (:cnt cnt :any-may-end any-may-end) (returned)
  "Finishes the entire thing, returns given value."
  (setf any-may-end t)
  `(progn (setf ,cnt 0)
	  (setf ret ,returned)
	  (return nil)))

;;Collecting/summing/etc.
(def-umac collect () (to &rest collected)
  "Collects to variable."
  (add-var to)
  `(setf- append ,to (list ,@collected)))

(def-umac append () (to &rest appended)
  "Appends to variable."
  (add-var to)
  `(setf- append ,to ,@appended))

(def-umac sum () (to &rest summed)
  "Sums to variable."
  (add-var (if (listp to) to `(,to 0)))
  `(setf- + ,(if (listp to) (car to) to) ,@summed))

(def-umac funcall () (to fun &rest args)
  "Function-call changing."
  (add-var to)
  `(setf to (funcall fun to args)))

(def-umac operation () (to operation &rest args)
  "Some arbitrary operation applied to something."
  (add-var to)
  `(setf- ,operation ,to ,@args))
