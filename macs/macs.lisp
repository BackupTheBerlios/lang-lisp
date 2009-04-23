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

(mac-add |quote| () (symbol)
  (cond
    ((symbolp symbol)
     (make-instance 'value :type `(eql ,symbol) :from symbol))
    (t
     (error "quote does not quote anything else then symbols yet."))))

(defun resolve-body (local body)
  (iter (for c in body)
	(collect (all-resolve local c))))

(mac-add |namespace| (:*local local) (name &rest body)
  (make-namespace :ns name :body (resolve-body local body)))

(mac-add |progn| (:*local local) (&rest body)
  (make-progn (resolve-body local body)))

(mac-add |let| (:*local local) ((&rest vars) &rest body)
  "Makes variables. Made in sequence. Rawmac since variables need\
 resolving. Uses :again to resolve body too."
  (let ((out-let (make-let (iter (for v in vars)
				  (collect
				      (if (listp v)
					  `(,(car v) 
					     ,(all-resolve local (cadr v)))
					  `(,v nil)))) nil)))
    (setf- update-local local out-let)
    (setf (slot-value out-let 'body) (resolve-body local body))
    out-let))




