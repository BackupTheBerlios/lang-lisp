;;
;;  Copyright (C) 2009-04-06 Jasper den Ouden.
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

(defclass local (namespace-local)
  ((state :initform nil :initarg state :accessor state)
   
   (variables :initform (list nil) :initarg :vars :type list)
   (functions :initform (list nil) :initarg :funs :type list)
   (macros :initform (list nil)    :initarg :macs :type list)
   (sym-macros :initform (list nil) :initarg :sym-macs :type list))
  (:documentation "Keeps track of local variable, functions, macros and\
 symbol-macros."))

(defun local-add (local &key vars funs macs sym-macs ns write-ns)
  "Makes new local state with added variables, functions, macros etc."
  (with-slots (variables functions macros sym-macros namespaces) local
    (make-instance 'local
      :vars (append vars (car variables))
      :funs (append funs (car functions))
      :macs (append macs (car macros))
      :sym-macs (append sym-macs (car sym-macros))
      :namespaces (append ns namespaces)
      :write-namespace (if-use write-ns (car ns)))))

(defun local-var-with-types (local vars types)
  "Adds variables with types."
  (local-add local
    :vars (iter
	    (for v in vars)
	    (for tp in types)
	    (collect (list v (make-instance 'value :type tp))))))

(defgeneric update-local (local added)
  (:documentation "Updates the local state with information from added."))

(defclass -progn ()
  ((body :initarg :body :type list)
   (out-type :initarg :out-type :initform nil :type list)))

(defmethod out-type ((body -progn));Resulting type of body memoized when used.
  (with-slots (body out-type) body
    (if-use out-type
	    (out-type (car(last body))))))

(defun make-progn (body)
  (make-instance '-progn :body body))

(defmethod update-local ((local local) (progn -progn))
  local)

(defclass -let (-progn)
  ((vars :initarg :vars :initform nil :type list)))

(defun make-let (vars body)
  (make-instance '-let :vars vars :body body))

(defmethod update-local ((local local) (let -let))
  (with-slots (vars body) let
    (local-add local :vars vars)))

(defclass -flet (-progn)
  ((vars :initarg :vars :initform nil :type list)))

(defun make-flet (funs body)
  (make-instance 'flet :funs funs :body body))

(defmethod update-local ((local local) (flet -flet))
  (with-slots (funs body) let
    (local-add local :funs funs)))


(defparameter *local* (make-instance 'local) "An defaultly empty local.")
