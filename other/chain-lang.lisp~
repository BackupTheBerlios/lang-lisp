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

(def-conv eval-getstr-code (getstr code :input getstr)
  (multiple-value-bind (final-str ret)
      (read-lisp getstr (if (stringp rest) rest "")
	(lambda (obj)
	  (cond
	    ((stringp obj) (if-use (parse-number obj) (intern obj)))
	    (t		obj))))
    (declare (ignorable final-str))
    ret))

(def-conv eval-str-code (str code)
  "Evaluate string to produce code."
  (eval-getstr-code (lambda () "") input))

(def-conv nil (stream getstr)
  "Makes a get-str based on a stream."
  (lambda () (read-line input)))

(def-conv eval-stream-code (stream code :input stream)
  "Read from stream, and evaluate to produce code."
  (eval-getstr-code (lambda () (read-line stream))))

(def-conv eval-file-code (file code :input filename)
  "Open stream at filename, read and evaluate to produce code."
  (with-open-file (stream filename :direction :input :if-does-not-exist nil)
    (when stream
      (eval-stream-code stream))))

(def-conv eval-code-res (code res)
  "Evaluate from token code to resolved."
  (fun-resolve input (getf rest :vars)
	       :state (if-use (getf rest :state) *state*)))

(def-conv eval-res-c (res c :input res)
  "Evaluate from resolved to C code."
  (process-code res :state (if-use (getf rest :state) *state*)
    :ps (make-ps :end-format-str (when (getf rest :fun-level) "return ~D")
		 :body-level (getf rest :body-level))))

(def-conv eval-res-sum (res sum)
  "From resolved code to more readable summary."
  (let ((more-on-fun (getf rest :more-on-fun))
	(more-on-mac (getf rest :more-on-mac)))
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
     input)))))

;;Make the chains.
(chain-convs '(stream getstr code res c))
(chain-convs '(stream getstr code res sum))
(chain-convs '(str getstr code res sum))
(chain-convs '(str getstr code res c))

;;Old version.
(defmacro a-evalm (from to arg &rest keys)
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
	    `(a-evalm code ,to (,eval ,arg) ,@keys)))))
      (code
       (case to
	 (res  `(eval-code-res ,arg
			       ,@(getkey :state) ,@(getkey :vars)))
	 
	 ((c sum)
	  `(a-evalm res ,to (eval-code-res ,arg
					 ,@(getkey :state) ,@(getkey :vars))
		  ,@keys))))
      (res
       (case to
	 (c    `(eval-res-c ,arg ,@(getkey :body-level) ,@(getkey :fun-level)))
	 (sum  `(eval-res-sum ,arg ,@(getkey :more-on-fun)
			      ,@(getkey :more-on-mac))))))))
