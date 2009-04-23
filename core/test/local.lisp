(in-package #:lang)

(load "core/local.lisp")

;Macro results go in these. (TODO code -> from?
(defclass out ()
  ((name  :initarg :name :initform nil))
  (:documentation "Holds information of final-macro output of Lang.
Usually as first element in list, with arguments following."))

(with-slots (variables) (update-local (make-instance 'local)
				     `(,(make-instance 'out :name 'body)
					(:vars '((cookie 1)))
					1 2 3))
  variables)

