;;
;;  Copyright (C) 2009-02-07 Jasper den Ouden.
;;
;;  This file is part of Lang(working title).
;;
;;  Lang is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Lang is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with Lang.  If not, see <http://www.gnu.org/licenses/>.
;;

(defpackage #:read
  (:use #:common-lisp #:generic)
  (:export get-token first-equal reader getstr-stream is-whitespace
	   clause-comment clause-line-comment clause-add))

(in-package #:read)

(defun getstr-stream (stream)
  "A getstr for functions taking one."
  (lambda () (read-line stream)))

(defun is-whitespace (ch)
  (case ch ((#\Newline #\Space #\Tab) t)))

(defun not-whitespace (ch)
  (not(is-whitespace ch)))

(defun non-symbol (ch)
  "Characters that are not part of symbols."
  (case ch ((#\Newline #\Space #\Tab #\) #\( #\|) t)))

(defun count-to-stop (str stopchar)
  (loop for el across str ;Iterate until ending of symbol.
        for i from 0
     when (funcall stopchar el)
     return i))

(defun get-token (str &optional (stopchar #'is-whitespace))
  "Gets a token from a string. Stopchar are characters it stops for, must \
are characters that must be in there, or it will also stop.
Returns: the string token, the ending index, the element it stopped at."
  (if-with i (count-to-stop str stopchar)
      (values (subseq str 0 i) i (when (< i (length str)) (aref str i)))
      (values str (length str) nil)))

(defun skip-token (str &optional (stopchar (lambda (ch)
					     (not (is-whitespace ch)))))
  (if-with i (count-to-stop str stopchar)
    (subseq str i) ""))

(defun subseq* (str start &optional end)
  "Subseq without bounds errors."
  (if (> (length str) start)
      (subseq str start (if end (min end (length str)) (length str)))
      ""))

(defun first-equal (eql-to str)
  "Sees if the first part of a string is equal to something."
  (cond ((= (length eql-to) 0)
	 t)
        ((= (length str) 0)
	 (= (length eql-to) 0))
        ((stringp eql-to)
	 (string= (subseq* str 0 (length eql-to)) eql-to))
	((characterp eql-to)
	 (eql (aref str 0) eql-to))))

(defun reader (str getstr whitespace clauses &key limit)
  "General reading, str is first string, getstr gets subsequent strings as \
needed, until it returns nil.
Clauses is pairs of expected words and the functions get the string beyond,
what these functions return is where reader continues."
  (do ((i 0 (+ i 1))
       (str str (if (= (length str) 0) (funcall getstr) str))
       (stop-now nil stop-now))
      ((or (when limit (>= i limit))
	   stop-now (= (length str) 0)) (values str stop-now))
    (cond
      ((funcall whitespace (aref str 0)) ;Skip whitespace.
       (setf- subseq* str 1))
      (t
       (loop for c in clauses ;Reponses for different encountered strings.
	 when (first-equal (car c) str)
	 return
	 (progn
	   (setf- subseq* str (length (car c)))
	   (multiple-value-bind (new-str stop) (funcall (cadr c) getstr str)
	     (case stop
	       (:skip-str
		(setf str (funcall getstr))
		(setf stop-now stop))
	       (t
		(setf stop-now stop)))
	     (setf str new-str))))))))

;;Clauses
(defun clause-comment (getstr &optional (str "") (end-comment-str "*/"))
  "Zips past the comment. TODO C has escapes for these?"
  (do ((str str (funcall getstr)))
      ((null str) nil)
    (when-with retstr
	(do ((str str (subseq* str 1))) ((= 0 (length str)) nil)
	  (when (first-equal str end-comment-str)
	    (return str)))
      retstr)))

(defun clause-line-comment (getstr &optional (str ""))
  "Zips past line comment. Currently assumes that things are read line by \
line."
  (declare (ignorable str))
  (funcall getstr))

(defun clause-add (add &key (stop #'non-symbol))
  "Produces an adding clause getting a symbol."
  (lambda (getstr str)
    (declare (ignorable getstr))
    (let ((got (get-token str stop)))
      (funcall add got)
      (subseq str (length got)))))

