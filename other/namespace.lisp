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

(defpackage #:namespace
  (:use #:common-lisp #:generic)
  (:export namespace-state
   ;Looks like i am accessing this a lot, is it bad?
    namespaces write-namespace
    namespace-symbol get-symbol get-symbol-assoc gen-c-name))

(in-package #:namespace)

(defclass namespace-state ()
 ;For generating symbols.
  ((gen-str :initform 'gen)
   (gen-cnt :initform 0)
   
   (namespaces :initform nil)
 ;The one new stuff should go to. (Use of it is in flet and such.)
   (write-namespace :initform nil)))

(defun append-namespace (namespace symbol)
  (intern (format nil "~D-~D" namespace symbol)))

(defun namespace-symbol (symbol state)
  "Adds current namespace to symbol."
  (with-slots (write-namespace) state
    (if (null write-namespace)
	symbol (append-namespace write-namespace symbol))))

;Get from hash table.
(defun get-symbol (symbol from state)
  "Gets a symbol from an hash table."
  (let (got)
    (loop for namespace in (slot-value state 'namespaces)
       until (setf got (gethash (append-namespace namespace symbol) from)))
    (if-use got (gethash symbol from))))

(defun (setf get-symbol) (to symbol from state)
  "Sets a symbol from an hash table."
  (setf (gethash (namespace-symbol symbol state) from) to))

;Get from association list.
(defun get-symbol-assoc (symbol from state)
  "Gets a symbol from an assoc list."
  (let ((from (car from)) got with-name)
    (loop for namespace in (slot-value state 'namespaces)
       until (setf got (assoc (setf with-name 
				    (append-namespace namespace symbol))
			      from)))
    (if got
	(values (cadr got) with-name)
	(values (cadr (assoc symbol from)) symbol))))

(defun (setf get-symbol-assoc) (to symbol from state)
  "Sets a symbol from an assoc list."
  (cond
    ((loop for namespace in (slot-value state 'namespaces)
	when (setf got (assoc (append-namespace namespace symbol) (car from)))
	return (setf (cadr (assoc (append-namespace namespace symbol)
				  (car from)))
		     to))
       to)
    ((assoc symbol (car from))
     (setf (cadr (assoc symbol (car from))) to))
    (t
     (push (list (namespace-symbol symbol state) to)
	   (car from)))))

(defun gen-c-name (state)
"Generates a name symbol."
;(NOTE if this doesnt play well with hash table, do something about it.)
  (with-slots (namespaces gen-str gen-cnt) state
    (setf- + gen-cnt 1)
    (intern(format nil "~D~D~D" (if-use (car namespaces) "")
		                gen-str gen-cnt))))
