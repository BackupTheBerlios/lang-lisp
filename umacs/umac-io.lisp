
(defun first-eql (cases str getstr)
  (cond
    ((listp cases)
     (case (length cases)
       (0 nil)
       (1 `(when (first-equal ,(car cases) ,str)
	     (setf- subseq ,str ,(length (car cases)))))
       (t `(or* ,@(loop for c in cases collect
		       `(when (first-equal ,c ,str)
			  (setf- subseq ,str (length ,c))))))))
    ((stringp cases)
     `(when (first-equal ,cases ,str)
	(setf- subseq ,str ,(length cases))))
    ((eql cases t)
     't)))

(def-umac read-case *umac-macs* ()
		 (str getstr &rest case-clauses)
  `(cond 
     ,@(loop for c in case-clauses collect
	 (argumentize-list (cases &rest body) c
	   `((first-eql ,str ,getstr ,cases)
	      (read ,str ,getstr (,@body)))))))
