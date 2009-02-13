(in-package #:lang)

(add-manual-type-generality *state*
  (lambda (type compare-type state)
    "Eql."
    (cond
     ;See if they have the mark of eql.
      ((or* (not (listp type)) (not (listp compare-type))
	    (not (and (eql (car type) '|eql|)
		      (eql (car compare-type) '|eql|))))
       nil)
      ((listp (cadr type)) ;General type still undefined eql.
       (let ((c (cadr compare-type)))
	 (cond ;And so is what we compare to, so they have to match up.
	   ((listp c)
	    (and (eql (car c)  (caadr type))
		 (eql (cadr c) (cadadr type))))
	   (t  ;What we compare to is entirely specified, see if in 
	       ;right class.
	    (case (caadr type)
	      (|integer| (integerp c))
	      (|number|  (numberp c))
	      (|symbol|  (and* (listp c)
			       (eql (car c) '|quote|)
			       (symbolp (cadr c)))))))))
      (t ;Both entirely set, must match.
       (eql (cadr type) (cadr compare-type))))))
