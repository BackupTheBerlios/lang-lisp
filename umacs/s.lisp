(load "../other/generic.lisp")
(load "../other/argumentize-list.lisp")

(load "umac.lisp")
(load "umac-basic.lisp")
(load "umac-loop.lisp")

(print 'a)

(in-package #:umac)

(setf *umac-macs* (make-hash-table))

(umac-fun *umac-macs* 'cnt 0 'sum '(ret 2 4))

(umac-fun *umac-macs* 'cnt 0 'series
	  '(name
	    (loop (sum a 1))
	    (sum val-0 4)
	    (sum ret 2 4)))

(umac () (repeat (j 5) (collect ret j))))

(umac () (loop
	   (while (< i 4))
	   (collect ret (umac () (repeat (j 5) (collect ret (list i j)))))
	   (sum i 1)))

(umac () (loop name
	    (for-range i 0 10)
	    (collect ret i)))

(argumentize-list (&key  (bla 1)) (list 1 4)
  bla)