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

(defun type-add-to-or (or-type-list add-tp)
  "Adds something to an (or ..) type."
  (append or-type-list
    (case (car add-tp)
      (or (loop for a in (cdr add-tp) collect (add-to-or got-orlist a)))
      (t  (unless (loop for tp in got-orlist
		     when (equalp add-tp tp) return t)
	    (list add-tp))))))

(rawmac-add cond () () (&rest clauses)
  (let (cond-res body-res body-type)
    (dolist (c clauses) ;Resolve them.
      (push (fun-resolve (car c) type-of :state state) cond-res)
      (push (fun-resolve `(progn ,@(cdr c)) type-of :state state) body-res)
      (setf- type-add-to-or body-res (out-type (car body-res))))
    `(,(make-instance 'out :type `(or ,@body-type))
       ,@(loop for c in cond-res
	    for b in body-res
	    collect `(,c ,b)))))

(mac-add if () () (question yes no)
  `(cond (,question ,yes) (true ,no)))

(mac-add when () () (cond &rest body)
  `(cond (,cond ,@body)))

(mac-add unless () () (cond &rest body)
  `(cond ((not ,cond) ,@body)))

;;TODO NOTE feel unsure..

(defun c-fork1-or- (type state type-of varsym varval body &key not-general)
  "C the fork1 lang macros."
  (with-fun-resolve
    (let*((res (resolve varval type-of))
	  (out-type (out-type res)))
      (case (car out-type)
	(or
	 (let (cases case-types types)
	   (dolist (tp (cdr out-type))
	     (unless (and not-general 
			  (loop for tp2 in (cdr out-type)
			     when (and (not (equalp tp tp2))
				       (type-more-general tp2 tp))
			     return t))
	       (push tp case-types)
	       (push (resolve `(progn ,@body) (cons (list varsym tp)
						    type-of))
		     cases)
	       (type-add-to-or types (out-type (car cases)))))
	   `(,(make-instance 'out :name 'fork-type1
			     :type (if (null (cdr types)) (car types)
				       `(or ,@types)))
	      ,type
	      ,varsym ,res
	      ,case-types ,cases)))
	(t ;Not even an or, just resolve.
	 (resolve `(let ((,varsym ,varval)) type-of)))))))

(rawmac-add fork1 () ((eql (quote or-type)))
    (type (varsym varval) &rest body)
  "If the type has an 'or', it splits them over the different functions for\
 them when possible."
  (c-fork1-or- type state type-of varsym varval body))

(rawmac-add fork1 () ((eql (quote or-type-not-general)))
    (type (varsym varval) &rest body)
 "Same as the or-type variant, except that this one put versions or types\
 that are more general then others together."
  (c-fork1-or- type state type-of varsym varval body :not-general t))

(mac-add fork () () (type (&rest vars) &rest body)
  "Fork for more variables."
  `(fork-type1 ,type ,(car vars)
     (fork-type ,type (,@(cdr vars))
       ,@body)))

