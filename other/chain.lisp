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

(in-package #:lang)

(defun parse-number (str)
  "Parses a number and returns either an integer or a float.
Nil if it doesn't recognize it." ;TODO put in proper file.
  (if-use
   (loop for ch across str
         for n from 0
      when (eql ch #\.)
      return (+ (parse-integer(subseq str 0 n))
		(* (parse-integer(subseq str (+ n 1)))
		   (expt 10.0 (- (+ n 1) (length str))))))
   (parse-integer str :junk-allowed t)))

;Previous function is needed for this one.
(defun eval-getstr-code (getstr)
  "Gets code from a series of strings, obtained with (funcall getstr)."
  (flet ((eql-curry (with)
	   (lambda (sym) (eql sym with))))
    (tokenlist-make-tree
     (let ((str ""))
       (loop while (setf str (funcall getstr))
	  append (tokenize-str str
		   :wrap (lambda (tok k)
			   (declare (ignorable k))
			   (if-use (parse-number tok) (intern tok)))
	           :except (list #\( #\) #\; #\|))))
     :list-open (eql-curry '|(|) :list-close (eql-curry '|)|)
     :sep (eql-curry '|;|) :sep-stop (eql-curry '|\||)
     :comment-start (eql-curry '|/*|) :comment-stop (eql-curry '|*/|)
     :next-lister (eql-curry '|'|))))

(defun eval-str-code (str)
  "Evaluate string to produce code."
  (eval-getstr-code (lambda () (let ((out str)) (setf str nil) out))))

(defun eval-stream-code (stream)
  "Read from stream, and evaluate to produce code."
  (eval-getstr-code (lambda () (read-line stream))))

(defun eval-file-code (filename)
  "Open stream at filename, read and evaluate to produce code."
  (with-open-file (stream filename :direction :input :if-does-not-exist nil)
    (when stream
      (eval-stream-code stream))))

(defun eval-code-res (code &key vars (state *state*))
  "Evaluate from token code to resolved."
  (fun-resolve code vars :state state))

(defun eval-res-c (res &key (state *state*)
		   (body-level t) fun-top top-level)
  "Evaluate from resolved to C code."
  (process-code res :state state :fun-top fun-top :top-level top-level
		:body-level body-level))

(defun eval-res-sum (input &key more-on-fun more-on-mac)
  "From resolved code to more readable summary."
  (flet ((summary (sub-input)
	   (eval-res-sum sub-input
		    :more-on-fun more-on-fun ::more-on-mac more-on-mac)))
  (case (type-of input)
    (cons
     (loop for el in input collect
	  (summary el)))
    (fun
     (with-slots (name arg-types out-type more-specific) input
       `(:fun ,name ,arg-types ,out-type
	      ,@(cond
		 (more-on-fun
		  `(:more-specific ,(summary more-specific)))))))
    (out
     (with-slots (name out-type code) input
       (append (list :out name out-type)
	       (when more-on-mac `(:code ,(summary code))))))
    (value
     (with-slots (out-type from) input
       `(:val ,(summary from) ,out-type)))
    (t
     input))))

(defmacro evalm (from to arg &rest keys)
  "Evaluates from some point to another, better use this instead
of the functions."
  (flet ((getkey (key) (when-with got (getf keys key) `(,key ,got))))
    (case from
      ((file stream str)
       (let ((eval (case from
		     (file   'eval-file-code)
		     (stream 'eval-stream-code)
		     (str    'eval-str-code))))
	 (case to
	   (code `(,eval ,arg))
	   ((res c sum)
	    `(evalm code ,to (,eval ,arg) ,@keys)))))
      (code
       (case to
	 (res  `(eval-code-res ,arg
			       ,@(getkey :state) ,@(getkey :vars)))
	 
	 ((c sum)
	  `(evalm res ,to (eval-code-res ,arg
					 ,@(getkey :state) ,@(getkey :vars))
		  ,@keys))))
      (res
       (case to
	 (c    `(eval-res-c ,arg ,@(getkey :body-level)
			    ,@(getkey :fun-top) ,@(getkey :top-level)))
	 (sum  `(eval-res-sum ,arg ,@(getkey :more-on-fun)
			      ,@(getkey :more-on-mac))))))))
