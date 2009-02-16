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

(load "test/util.lisp")

(defun eql-curry (with)
  (lambda (sym) (eql sym with)))

;Unit test.
;;TODO current test passed, expand the test to include sep and sep-stop?
(loop repeat 1000
   unless
   (let ((tree (random-tree 0.2 3 3 :top t 
		 :from-symbols (loop repeat 20 collect (gensym)))))
     (equalp
      tree
      (car(tokenlist-make-tree
	   (tokenize-str (format nil "~D" tree)
			 :wrap (lambda (tok k) (intern (string-upcase tok)))
			 :except (list #\( #\) ))
	   :list-open (eql-curry '|(|) :list-close (eql-curry '|)|)))))
   return (error "One of the written random trees didn't get read back
 correctly."))

;Playtests.
(tokenlist-make-tree
 (tokenize-str "(progn
 (defun sqr (x (number);) | * x x ;)
 (defun meh (a (int); b (int)) |
   + a (* 2 a b);)"
	       :except (list #\( #\) #\; #\|)))

(tokenlist-make-tree
 (tokenize-str "(a b  (* x) x 43 1  6)"
	       :except (list #\( #\) #\; #\|)))


