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

(defun summary (input &key more-on-fun more-on-mac)
"Makes the resolved code more readable."
  (flet ((summary (sub-input)
	   (summary sub-input
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

(defmacro print-summary (input &rest stuff)
  "Prints the result from list-summary."
  `(print(summary ,input ,@stuff)))

(defun eql-curry (with)
  (lambda (sym) (eql sym with)))

(defun parse-number (str)
  "Parses a number and returns either an integer or a float.
Nil if it doesn't recognize it."
  (if-use
   (loop for ch across str
         for n from 0
      when (eql ch #\.)
      return (+ (parse-integer(subseq str 0 n))
		(* (parse-integer(subseq str (+ n 1)))
		   (expt 10.0 (- (+ n 1) (length str))))))
   (parse-integer str :junk-allowed t)))

;;TODO make it proper for non-playtest.
(defun eval-str-fun (type str &key vars (state *state*)
		     more-on-fun more-on-mac
		     (body-level t) fun-top top-level)
  "Evaluates a string, what it does with it depends on type. The keywords\
 correspond to those on resolve and process-code.
:tokenize Means just tokenize it into a tree.
:resolve  Resolves the tree with fun-resolve
:summary  Produces a summary of the resolved result.
:to-c     Produces C code."
  (let*((tree
	 (tokenlist-make-tree
	  (tokenize-str str
	    :wrap (lambda (tok k)
		    (declare (ignorable k))
		    (if-use (parse-number tok) (intern tok)))
	    :except (list #\( #\) #\; #\|))
	  :list-open (eql-curry '|(|) :list-close (eql-curry '|)|)
	  :sep (eql-curry '|;|) :sep-stop (eql-curry '|\||)
	  :comment-start (eql-curry '|/*|) :comment-stop (eql-curry '|*/|)
	  :next-lister (eql-curry '|'|)))
	(res (case type
	       (:tokenize nil)
	       (t (fun-resolve tree vars :state state)))))
    (case type
      (:tokenize
       tree)
      (:resolve
       res)
      (:summary
       (summary res :more-on-fun more-on-fun :more-on-mac more-on-mac))
      (:to-c
       (process-code res :state state :fun-top fun-top :top-level top-level
		     :body-level body-level)))))

(defmacro eval-str ((type &rest rest) str)
  "Wrapper for eval-str-fun."
  `(eval-str-fun ,type ,str ,@rest))
