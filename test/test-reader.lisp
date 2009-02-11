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

(random-tree 0.2 5 5)

;;TODO make the read/writes to a file in memory instead of hard disk.
;; WARNING Some storage isn't meant to write to like this.
(loop repeat 1000
   unless
   (let ((tree (random-tree 0.2 3 3 :top t 
		 :from-symbols (loop repeat 20 collect (gensym)))))
     (with-open-file (file "test/test-reader-file" :direction :output
		       :if-exists :supersede :if-does-not-exist :create)
       (format file "~D~%" tree))
     (with-open-file (file "test/test-reader-file" :direction :input)
       (equalp tree (car(tokenize-stream file :symbol-ize t)))))
   return (error "One of the written random trees didn't get read back
 correctly."))
