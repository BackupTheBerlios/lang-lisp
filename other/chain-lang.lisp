;;
;;  Copyright (C) 2009-04-03 Jasper den Ouden.
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
   (iter (for ch in-vector str)
         (for n from 0)
	 (when (eql ch #\.)
	   (return (+ (parse-integer(subseq str 0 n))
		      (* (parse-integer(subseq str (+ n 1)))
			 (expt 10.0 (- (+ n 1) (length str))))))))
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
  (eval-getstr-code (lambda () nil) input))

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
  (all-resolve (if-use (getf rest :local) *local*) input))

(def-conv eval-res-c (res c :input res)
  "Evaluate from resolved to C code." ;TODO intermediate 'res-funarg-debody?
  (destructuring-bind (&key (return :same)) rest
    (c-body-ize (conv-code (code-funarg-debody res)
			   (conv-c-state-changed *c-conv-state*
						 :return return)))))

(def-conv eval-res-lisp (res lisp)
  (conv-code input *conv-lisp-macs*))

(def-conv eval-res-eval-lisp (lisp eval-lisp)
  (eval input))

(def-conv eval-res-sum (res sum)
  "From resolved code to more readable summary."
  (destructuring-bind (&key more-on-fun more-on-mac) rest
    (flet ((summary (sub-input)
	     (evalm res sum sub-input))
	   (non-list-whine (must-be-list)
	     (unless (listp must-be-list)
	       (error "Iterate should be whining, but i guess i should \
instead. ~D is not a list." must-be-list))))
      (case (type-of input)
	(cons
	 (iter (for el in input)
	       (collect (summary el))))
	(applied-fun
	 (with-slots (fun out-type args) input
	 `(:fun ,(slot-value fun 'name) ,out-type 
	   :a ,(eval-res-sum args rest))))
	(-progn
	 (with-slots (body) input
	   (non-list-whine body)
	   `(:progn ,@(eval-res-sum body rest))))
	(-let
	 (with-slots (vars body) input
	   (non-list-whine body)
	   `(:let ,(when vars
		    (iter (for v in vars)
			  (collect (list (car v)
					 (eval-res-sum (cadr v) rest)))))
	      ,@(eval-res-sum body rest))))
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
(chain-convs '(str getstr code res lisp eval-lisp))

