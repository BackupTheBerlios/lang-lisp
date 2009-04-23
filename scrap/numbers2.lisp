
(in-package #:lang)

(defun do-op (op max-op min-op a b)
  (cond
    ((numberp a)
     (cond
       (((or (symbolp) (numberp b))
	(funcal op a b))
       ((listp b)
	(case (car b)
	  (|eql|
	   `(|eql| ,(do-op op a b)))
	  (|number|
	   (let ((fr (do-op op a (cadr b)))
		 (to (do-op op a (caddr b))))
	     `(|number| ,(funcall max-op fr to) ,(funcall min-op fr to))))))
       (t
	(error "Dont know this."))))
    ((or (symbolp b) (numberp b))
     (do-op b a))
    ((listp a)
     (unless (listp b)
       (error "Dont know this."))
     (case (car a)
       (|eql|
	(case (car b)
	  (|eql|
	   `(|eql| ,(do-op op (cadr a) (cadr b))))
	  (|number|
	   (let ((ff 
	   `(|number| ,(do-op op (cadr a) (cadr b))
		      ,(do-op op (cadr a) (cadr b))
