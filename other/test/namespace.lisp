
(require :iterate)
(in-package #:namespace)

(let ((hash '(((dud 1))));(make-hash-table))
      (local (make-instance 'namespace-local)))
  (with-slots (namespaces write-namespace) local 
    (setf namespaces '(aa bb))
    (setf write-namespace 'bb))
  (setf (getassoc-symbol 'one hash local) 1)
  (setf (getassoc-symbol 'two hash local) 2)
  (values (getassoc-symbol 'two hash local)
	  (namespace-symbol local 'two)))

(let ((hash (make-hash-table))
      (alist nil)
      (local (make-instance 'namespace-local))
      (state (make-instance 'namespace-state))
      (namespace-list (iter (repeat 5) (gensym))))
  (let ((namespace-levels 10)
	(cnt 0))
    (flet ((cnt) (setf cnt (+ cnt 1)))
      (dotimes (i namespace-levels)
	(with-slots (namespaces write-namespace) local
	  (setf write-namespace (gensym))
	(dotimes (j 10)
	  (let ((cur (cnt)))
	    (
	(dotimes (j 10)
	
    
