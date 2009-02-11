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
;TODO this is based on pretty old stuff, find a better way to
;'single-tokenize', replace dos with loops(or iterates) where appropriate.

(defpackage #:reader
  (:use #:common-lisp #:generic)
  (:export tokenize-stream
	   strwrap str char-cnt line-cnt
	   strwrap-str strwrap-char-cnt strwrap-line-cnt))

(in-package #:reader)

(defun single-tokenize (stream stopchars &optional (startch ""))
  "Gets the next token, stops at any of the stopchars.
 (Even if nothing is found yet.)"
  (do ((out "" (format nil "~D~D" out ch)) 
       (ch startch (read-char stream nil nil)))
      ((or (in-list stopchars ch) (not ch))
       (progn (unread-char ch stream) out))))

(defstruct strwrap
  str (char-cnt 0 :type integer) (line-cnt 0 :type integer))

;And what about comments aswell?
(defun tokenize-stream (stream &key
	(list-open (list #\( )) (list-close (list #\) ))
	(whitespace (list #\Tab #\Newline #\Space)) (comment (list #\#))
	(except nil) (except-prev-nonsymbol nil) wrap symbol-ize
	(curchar-cnt 0) (curline-cnt 0))
  "Tokenizes in tree form, making the elements strings. returns a list \
with the tokenized tree form, character count at end, line count at end \
arguments: stream, and keyword arguments:
* list-open, list-close: lists of what starts and stops sub-list, default \
  (list #\() (list #\))
* except: these characters will be listed separately, alone. They need not \
  be surrounded by whitespace. Defaultly nil
* wrapper: Function that is used on any strings in the returned. This \
allows you to add extra information. Default is no wrap, just strings. Used\
 as (funcall wrapper string char-cnt line-cnt)
TODO character count not correct" ;TODO
  (let ((out nil) (done nil) (ch (read-char stream nil nil))
	(char-cnt curchar-cnt) (line-cnt curline-cnt))
  (flet ((wrap (str)
	   (let ((sym (if symbol-ize (intern str) str)))
	     (if wrap
	       (make-strwrap :str sym :char-cnt char-cnt :line-cnt line-cnt)
	       sym)))
	 (tokenize ()
	   (tokenize-stream stream
	     :list-open list-open :list-close list-close
 	     :except except :except-prev-nonsymbol except-prev-nonsymbol
	     :wrap wrap :symbol-ize symbol-ize
	     :curchar-cnt char-cnt :curline-cnt line-cnt)))
;while no done signal and the characters keep coming
    (do () ((or done (not ch)) (values out char-cnt line-cnt))
    ;Perhaps this should be cons, but that would collect nils too.
      (setf- append out
	(cond
       ;Skip whitespace
	 ((in-list whitespace ch)
	  nil)
       ;Skip commented stuff.
	 ((in-list comment ch)
	  (do ()
	      ((when (setf ch (read-char stream nil nil))
		 (case ch
		   (#\Newline nil)
		   (t t)))
		  nil)))
       ;Opening and closing sublists.
         ((in-list list-open ch)
	  (multiple-value-bind (list n-char-cnt n-line-cnt) (tokenize)
	    (setf char-cnt n-char-cnt)
	    (setf line-cnt n-line-cnt)
	    (list list)))
         ((in-list list-close ch)
	  (setf done t) nil)
       ;Exceptions which are by themselves.
         ((or (in-list except-prev-nonsymbol ch) (in-list except ch))
	  (list ch))
       ;Just gather symbols.
	 (t
	  (let*((str
		 (single-tokenize stream
		   (append list-open list-close whitespace except comment)
		   ch))
		(out (list (wrap str))))
	    (setf- + char-cnt (length str))
	    out))))
     (unless done
       (setf- + char-cnt 1)
	 (when (eql ch #\Newline)
	   (setf char-cnt 0)
	   (setf- + line-cnt 1))
	 (setf ch (read-char stream nil nil)))))))


