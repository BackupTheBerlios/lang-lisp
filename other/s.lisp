
(load "other/generic.lisp")
(load "other/read.lisp")

(in-package #:read)

(defun non-symbol (ch)
  (case ch ((#\Newline #\Space #\Tab #\) #\() t)))

(defun add-clause (&key (stop #'non-symbol))
  "Produces an adding clause getting a symbol."
  (lambda (getstr str)
    (let ((got (get-token str stop)))
      (funcall add got)
      (subseq str (length got)))))

(defun eval-getstr-code (getstr &key (str ""))
  (let (out)
  (flet ((add-out (added)
	   (push added out)))
    (values
     (reader str getstr
       `(("/*" ,#'read:read-comment) ("//" ,#'read:read-line-comment)
	 ("("  ,(lambda (getstr str)
		  (multiple-value-bind (newstr add)
		      (eval-getstr-code getstr str)		    
		    (add-out add)
		    newstr)))
	 (")"  ,(lambda (getstr str)
		  (values str t)))
	 ("<:"  ,(lambda (getstr str)
		   (let (first rest (stage :first) (n 0))
		   (flet ((add-here (added)
			    (case stage
			      (:first (push added first))
			      (:body  (push added rest))
			      (:rep
			       (unless (string= (nth n first)
						added)
				 (error "Rep not repeating properly!")
				 (setf- + n 1))))))
		    (reader str getstr
		       `(("/*" ,#'read:read-comment)
			 ("//" ,#'read:read-line-comment)
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
			 (""   ,(add-clause :add #'add-here)
		    (add-out (append first rest))
		    str))
	 (""   ,(add-clause :add #'add-out))

     (reverse out)))))

(print 'a)
(eval-getstr-code (lambda () "") :str "  (3 4  (6 8) 9)")
