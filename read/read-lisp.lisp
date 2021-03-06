
(defpackage #:read-lisp
  (:use #:common-lisp #:generic #:read)
  (:export read-lisp clause-add-xml-like))

(in-package #:read-lisp)

(defun non-symbol (ch)
  "Characters that are not part of symbols."
  (case ch ((#\Newline #\Space #\Tab #\) #\( #\| #\;) t)))

(defun clause-add-xml-like (final-add)
  "Adds a xml-like reading. TODO untested"
  (lambda (getstr str)
    (let (first rest (stage :first) (n 0))
      (flet ((add-here (added)
	       (case stage
		 (:first (push added first))
		 (:body  (push added rest))
		 (:rep
		  (unless (string= (nth n first) added)
		    (error "Rep not repeating properly!")
		    (setf- + n 1))))))
	(reader str getstr
	  `(("/*" ,#'clause-comment)
	    ("//" ,#'clause-line-comment)
	    (">"  ,(lambda (getstr str)
		     (case stage
		       (:first (setf stage :body)
			       (setf- reverse first)
			       str)
		       (:body  (add-here ">")
			       str)
		       (:rep   (values str t)))))
	    ("</" ,(lambda (getstr str)
		     (case stage
		       (:body  (setf stage :rep)
			       (setf- reverse rest)
			       str)
		       (t      (add-here "</")
			       str))))
	    (""   ,(clause-add #'add-here :stop #'non-symbol)))))
      (funcall final-add (append first rest))
      str)))

(defun read-lisp (getstr &optional (str "") (symbol-process #'identity))
  "Reads lisp-like stuff."
  (let (non-sub-out out is-sep)
  (flet ((add-out (added)
	   (setf added (funcall symbol-process added))
	   (if is-sep
	     (setf (car(last out)) `(,@(car(last out)) ,added))
	     (setf out             `(,@out ,added)))))
    (let ((newstr
      (reader str getstr
       `(("/*" ,#'clause-comment) ("//" ,#'clause-line-comment)
	 ("("  ,(lambda (getstr str)
		  (multiple-value-bind (newstr add)
		      (read-lisp getstr str symbol-process)		    
		    (add-out add)
		    newstr)))
	 ("<:" ,(clause-add-xml-like #'add-out))
	 (")"  ,(lambda (getstr str)
		  (values str t)))
	 ("|"  ,(lambda (getstr str)
		  (setf- append non-sub-out out)
		  (setf out (list))
		  (setf is-sep nil)
		  str))
	 (";"  ,(lambda (getstr str)
		  (unless is-sep
		    (setf is-sep t)
		    (setf out (list out)))
		  (setf out `(,@out ,(list)))
		  str))
	 (""   ,(clause-add #'add-out :stop #'non-symbol))))))
      (when (and is-sep (null(car(last out))))
	(setf out (reverse(cdr (reverse out)))))
      (values newstr (append non-sub-out out))))))
