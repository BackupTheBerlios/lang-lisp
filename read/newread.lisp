
(defpackage #:str-read
  (:use #:common-lisp #:iterate)
  (:export reader reader-from-stream reader-from-string
	   reader-forward reader-next-str=
	   skip-characters skip-whitespace)
  (:documentation "Package to read strings."))

(in-package #:str-read)

(defclass reader ()
  ((getstr :type function :initarg :getstr :initform (lambda() nil))
   (str    :type (or null string) :initarg :str :initform "")))

(defun reader-from-stream (stream &optional (first-str ""))
  "Make reader from stream. (It then owns your stream, dont touch it.)"
  (make-instance 'reader
    :getstr (lambda () (when-with rline (read-line stream nil nil)
			 (concatenate string (vector #\Newline) rline)))
    :str first-str))

(defun reader-from-string (string)
  "Make reader from string."
  (make-instance 'reader :str string))

(defun reader-getstr (reader)
  (when-with got (funcall getstr)
    (setf str (concatenate 'string str got))
    t))

(defun reader-forward-unassured (reader character-cnt)
  (with-slots (str) reader
    (setf str (subseq str character-cnt))))

(defun reader-assure-length (reader want-length)
  "Assures we have the length of string wanted.\
 Returns t if success, nil else."
  (do () ((>= (length str) (length string)) t)
    (unless (reader-getstr reader)
      (return nil))))

(defun reader-assure-have-character (reader characters)
  "Assures the currently read string contains one of the characters."
  (with-slots (str) reader
    (do ((i 0 (+ i 1)))
	((when (>= i (length str)) (not(reader-getstr reader))) nil)
      (when (funcall characters (aref str i))
	(return i)))))

(defun reader-forward (reader character-cnt)
  "Moves the reader forward. returns whether success"
  (with-slots (str getstr) reader
    (when (reader-assure-length reader character-cnt)
      (reader-forward-unassured reader character-cnt)
      t)))

(defun reader-next-str= (reader string &optional (when-true :nothing))
  "Sees if next-be-read is equal to string"
  (when (characterp string)
    (setf string (format nil "~D" string)))
  (with-slots (str getstr) reader
    (when (reader-assure-length reader (length string))
      ;Should have enough.
      (when (string= str string 0 (length string))
	(case when-true
	  (:nothing t)
	  (:skip    (reader-forward-unassured reader (length string))))
	t))))

(defun cond-reader-next-str= (reader &rest clauses)
  (let ((rdr (gensym)))
    `(let ((,rdr ,reader))
       (cond
	 ,@(iter
	    (for c in clauses)
	    (collect
		(destructuring-bind ((string &optional when-true)
				     &body body) c
		  `(,(if (listp string)
			 `(or
			   ,@(iter
			      (for str in string)
			      (collect
				  `(reader-next-str= ,rdr ,str ,when-true))))
			 `(reader-next-str= ,rdr ,string ,when-true))))))))))

(defun whitespace (char)
  (case char ((#\Space #\Tab #\Newline) t)))

(defun read-token (reader
		     &optional (after :nothing) (ended-by #'whitespace))
  "Reads a token. (defaultly ended-by whitespace)"
  (with-slots (str) reader
    (let*((i (reader-assure-have-character reader ended-by))
	  (out-str (if i (subseq str 0 i) str)))
      (case after
	(:nothing nil)
	(:skip
	 (if i (reader-forward-unassured reader i)
	       (setf str ""))))
      out-str)))

(defun reader-skip (reader &optional (characters #'whitespace))
  "Skips the given characters. characters is a function, list or string"
  (with-slots (str getstr) reader
    (flet ((is-stop-char (len)
	     (when (= len 0)
	       (unless (reader-getstr reader) (return nil)))
	     (funcall characters (aref str 0))))
      (do ((len (length str) (- len 1)))
	  ((is-stop-char len)
	   t)
	(reader-forward-unassured reader 1)))))

(defun reader-at-end (reader)
  "If no string left, it tries to get it, if that fails, it is at the end."
  (with-slots (str getstr) reader
    (when (= (length str) 0)
      (reader-getstr reader))))

(defun reader-skip-to (reader end-str)
  "Reads until it finds the string."
  (when (characterp end-str)
    (setf end-str (format nil "~D" end-str)))
  (do () ((reader-next-str= reader end-str :skip) t)
    (unless (reader-forward reader 1) (return nil))))

(defclass clause ()
  ((when :initarg :when :type function)
   (do   :initarg :do :type function)))

(defun reader-clause (when do)
  "Creates a clause. A list of these is passed into reader-with-clauses,\
 when when returns true, do is run."
  (make-instance 'clause :do do
    :when (cond
	    ((or (stringp when) (characterp when))
	     (lambda (reader)
	       (reader-next-str= reader when :skip)))
	    ((listp when)
	     (lambda (reader)
	       (dolist (str when)
		 (when (reader-next-str= reader when :skip)
		   (return t)))))
	    ((functionp when)
	     when))))

(defun reader-with-clauses (reader clauses result)
  "Does something for clauses."
  (dolist (c clauses)
    (with-slots (when do) c
      (when (funcall when reader)
	(return (funcall do reader result))))))
