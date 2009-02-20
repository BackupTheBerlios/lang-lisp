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

(defpackage #:reader
  (:use #:common-lisp #:generic)
  (:export tokenize-str tokenize-stream tokenize-file
	   tokenlist-make-tree))

(in-package #:reader)

(defun get-token (str stopchar &optional must)
  "Gets a token from a string. Stopchar are characters it stops for, must \
are characters that must be in there, or it will also stop.
Returns: the string token, the ending index, the element it stopped at."
  (if-with i (loop for el across str ;Iterate until ending of symbol.
		   for i from 0
		when (or (in-list stopchar el) (and must (not(in-list must el))))
		return i)
      (values (subseq str 0 i) i (when (< i (length str)) (aref str i)))
      (values str (length str) nil)))

(defun no-wrap (tok k &optional n)
  (declare (ignorable k n))
  tok)

(defun tokenize-str (str &key (wrap #'no-wrap)
		          (whitespace (list #\Tab #\Space #\Newline))
 		          (comment (list #\#)) except except-prev)
  "Makes a list with tokens. Wrap takes as second argument the index of \
the first element.
Whitespace are the token-sepating characters.(Otherwise ignored.)
Comments end tokenization for the _entire_ string. \
   (tokenize-stream feeds them line by line, so that is how that works.)
Except are characters that finish whatever is being tokenized and form \
tokens by themselves when found.
Except-prev also form tokens by themselves but only if they are preceded \
by whitespace."
  (let ((k 0) out)
    (flet ((add (tok)
	     (unless (= (length tok) 0)
	       (push (funcall wrap tok k) out)))
	   (forward (n) (setf- + k n)
		        (setf- subseq str n)))
      (do ((j 0 (+ j 1))) ;Go word for word, until find comment sign, or end of str.
	  ((or* (= (length str) 0) (> j 50)
	     (if-use
	      (progn
  	      ;See if this is a preceding exception.
		(when (in-list except-prev (aref str 0))
		  (add (format nil "~D" (aref str 0)))
		  (forward 1))
   	      ;Iterate until next symbol
		(multiple-value-bind (tok i ch)		    
		    (get-token str nil (append whitespace comment))
		  (declare (ignorable tok))
		  (forward i)
		  (in-list comment ch)))
	    ;Get token.
	      (multiple-value-bind (tok i ch)
		  (get-token str (append comment whitespace except))
		(add tok)
		(forward (cond
			   ((in-list except ch)
			    (add (format nil "~D" ch))
			    (+ i 1))
			   (t
			    i)))
		(in-list comment ch))))
	   t)))
    (reverse out)))

(defun tokenize-stream (stream
       &key first-str (wrap #'no-wrap)
            (whitespace (list #\Tab #\Space #\Newline))
	    (comment (list #\#))
	    (except nil) (except-prev nil))
  "Tokenize for a stream. Wrapper for tokenize-str."
  (do ((n 0 (+ n 1))
       (str (if-use first-str (read-line stream)) (read-line stream))
       (out nil (append out
		 (tokenize-str str
		   :wrap (lambda (tok k) (funcall wrap tok k n))
		   :whitespace whitespace :comment comment
		   :except except :except-prev except-prev))))
      ((null str) out)))

(defun tokenize-file (filename 
       &key first-str (wrap #'no-wrap)
            (whitespace (list #\Tab #\Space #\Newline))
	    (comment (list #\#))
  	    (except nil) (except-prev nil))
  "Tokenize the stream of the file name. Wrapper for tokenize-stream."
  (with-open-file (stream filename :direction :input :if-does-not-exist nil)
    (when stream
      (tokenize-stream stream :first-str first-str :wrap wrap
	    :whitespace whitespace
	    :comment comment :except except :except-prev except-prev))))

(defun string=-curry (with)
  (lambda (str) (string= str with)))

(defun tokenlist-make-tree (from &key
	 (list-open (string=-curry "(")) (list-close (string=-curry ")"))
	 (comment-start (string=-curry ".")) (comment-stop (string=-curry "."))
	 (sep (string=-curry ";")) (sep-stop (string=-curry "|"))
	 (next-lister (string=-curry "'"))
	 cnt)
  "Makes a tree with a list of tokens."
  (let* (out sep-out is-sep)
    (flet ((make-tree (list &key cnt)
	     (tokenlist-make-tree list
  	         :list-open list-open :list-close list-close
	         :comment-start comment-start :comment-stop comment-stop
  	         :sep sep :sep-stop sep-stop :next-lister next-lister
		 :cnt cnt))
	   (is-sep-first ()
	     ;TODO could be more efficient. (Not really high priority.)
	     "Scans forward for separators."
	     (let ((n 0))
	       (dolist (el from)
		 (cond
		   ((and (= n 0) (funcall sep el))
		    (return t))
		   ((and (= n 0) (funcall sep-stop el))
		    (return nil))
		   ((funcall list-open el)
		    (setf- + n 1))
		   ((funcall list-close el)
		    (setf- - n 1)
		    (when (< n 0) (return nil)))))))
	   (add (token &optional force-normal)
	     "Adds a token/sublist."
	     (cond
	       ((and is-sep (not force-normal))
		(setf sep-out `(,@sep-out ,token)))
	       (t
		(setf out `(,@out ,token))))))
      (setf is-sep (is-sep-first))
      (do ((n 0 (+ n 1))) (nil out)
	(cond
        ;End of list and closings return the function.
	  ((or* (null from)
		(funcall list-close (car from))
		(when cnt (< n cnt)))
	   (when (and is-sep (not(null sep-out))) (add sep-out t))
	   (return (values out (cdr from))))
        ;Openings.
	  ((funcall list-open (car from))
	   (multiple-value-bind (result list-left) (make-tree (cdr from))
	     (setf from list-left)
	     (add result)))
        ;Comment starting and stopping.
	  ((funcall comment-start (car from))
	   (setf- cdr from)
	   (do () ((funcall comment-stop (car from)) nil)
	     (setf- cdr from)
	     (when (null from) (return out))))
        ;Separators.
	  ((funcall sep (car from))
	   (unless is-sep
	     (error "is-sep should always be expected."))
	   (setf- cdr from)
	   (add sep-out t)
	   (setf sep-out nil))
        ;Separator-stoppers.
	  ((funcall sep-stop (car from))
	    (when is-sep
	      (add sep-out)
	      (setf sep-out nil))
	    (setf- cdr from)
	    (setf is-sep (is-sep-first)))
	;Next-lister.
	  ((funcall next-lister (car from))
	   (let ((cnt (funcall next-lister (car from))))
	     (multiple-value-bind (result list-left)
		 (make-tree (cdr from) :cnt (if (integerp cnt) cnt 1))
	       (setf from list-left)
	       (add (cons (car from) result)))))
        ;Regular getting of tokens.
	  (t
	   (add (car from))
	   (setf- cdr from)))))))

