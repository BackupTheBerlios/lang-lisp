;;
;;  Copyright (C) 2009-04-08 Jasper den Ouden.
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
  (:use #:common-lisp #:generic #:iterate #:documented)
  (:export *namespace-hash* namespace-local
	   namespaces write-namespace
	   symbol-exported-p
	   namespace-symbol symget
	   gen-symbol))

(in-package #:namespace)

(defclass namespace (documented)
  ((export :documentation "What is exported. If :all, all, otherwise list\
 of symbols."
	   :initform :all :initarg :export-manner)
   (not-export :documentation "What is not exported, overrides "))
  (:documentation "A namespace"))

(defun symbol-exported-p (namespace symbol)
  "Returns true if the symbol is exported."
  (if (null namespace) t
      (with-slots (export not-export) namespace
	(or (and (eql export :all)
		 (not (in-list not-export symbol)))
	    (in-list export symbol)))))

(defvar *namespace-hash* (make-hash-table) "All current namespaces.")

(defclass namespace-local ()
  ((namespaces :documentation "Namespaces in the order we try them for a\
 match" :initform nil :initarg :namespaces)
   (write-namespace :documentation "Namespace that currently is written to."
		    :initform nil :initarg :write-namespace)
   (gen-str :documentation "Prepended before number of namespace."
	    :initform 'gen)
   (gen-cnt :initform 0)))

(defun prepend-name (name symbol)
  (intern (format nil "~D:~D" name symbol)))

(defun namespace-symbol (local symbol)
  "Adds current namespace to symbol."
  (with-slots (write-namespace) local
    (if (null write-namespace)
	symbol (prepend-name write-namespace symbol))))

(defgeneric symget (local from symbol)
  (:documentation "Gets a symbol."))

;Get from hash table.
(defun gethash-symbol (local from symbol &key set-to)
  "Gets a symbol from an hash table."
  (if-use
   (with-slots (namespaces) local
     (iter ;Search namespaces.
       (for prepend in namespaces)
       (when-with got (gethash (prepend-name prepend symbol) from)
	 (when (namespace-exported-p (gethash namespace *namespace-hash*))
	   (return
	     (if set-to
		 (setf (gethash (prepend-name prepend symbol) from) set-to)
		 got))))))
   (if set-to ;It was in namespace-less.(Those are all exported.)
     (setf (gethash symbol from) set-to)
     (gethash symbol from))))

(defmethod symget ((local namespace-local) (hash hash-table) (symbol symbol))
  (gethash-symbol local hash symbol))

(defmethod (setf symget) (to (local namespace-local) (hash hash-table)
		      (symbol symbol))
  "Sets a symbol from an hash table. TODO need to check stuff, like if\
 exported."
  (gethash-symbol local hash symbol :set-to to))

;Get from association list.
(defmethod symget ((local namespace-local) (from list) (symbol symbol))
  "Gets a symbol from an assoc list."
  (if-use (iter (for name in (slot-value local 'namespaces))
		(let* ((with-name (prepend-name name symbol))
		       (got-assoc (assoc with-name (car from))))
		  (when got-assoc
		    (return (values (cadr got-assoc) got-assoc with-name)))))
	  (let ((got-assoc (assoc symbol (car from))))
	    (values (cadr got-assoc) got-assoc symbol))))

(defmethod (setf symget) (to (local namespace-local) (from list)
			  (symbol symbol))
  "Sets a symbol from an assoc list. NOTE try not use it."
  (multiple-value-bind (got item) (symget local from symbol)
    (if item
	(setf (cadr item) to)
	(push (list (namespace-symbol local symbol) to)
	      (car from)))))

(defun gen-symbol (local)
"Generates a name symbol."
;(NOTE if this doesnt play well with hash table, do something about it.)
  (with-slots (gen-str gen-cnt write-namespace) local
    (setf- + gen-cnt 1)
    (intern(format nil "~D~D~D" (if-use write-namespace "")
		                gen-str gen-cnt))))
