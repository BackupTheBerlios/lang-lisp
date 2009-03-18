
(in-package #:argument)

;;Unit tests.

(defun rnd () (random 1.0))

(defun random-arg-list (from-pairs &key (deepen-chance 0.4)(stop-chance 0.4)
		       (top t) (depth 0) (max-depth 10))
  "Creates a random argument list using all the pair. 
NOTE chances are not really 'precisely defined'."
  (do ((out nil
	(cons ;TODO add rest, key, optional, do from-pair generation 
	      ;internally/via callback.
	 (cond
	   ((and (< (rnd) deepen-chance) (not (> depth max-depth)))
	     (multiple-value-bind (with-out new-pairs)
		 (random-arg-list from-pairs
		   :deepen-chance deepen-chance :stop-chance stop-chance
		   :top nil :max-depth max-depth :depth (+ depth 1))
	       (setf from-pairs new-pairs)
	       with-out))
	   (t
	    (let ((out (car from-pairs)))
	      (setf- cdr from-pairs)
	      out)))
	 out)))
      ((or (null from-pairs) (unless top (< (rnd) stop-chance)))
       (values out from-pairs))))

(defun split-ends (tree splitter)
  (cond
    ((listp tree)
     (let (out1 out2)
       (dolist (el tree)
	 (multiple-value-bind (v1 v2) (split-ends el splitter)
	   (push v1 out1)
	   (push v2 out2)))
       (values out1 out2)))
    (t
     (funcall splitter tree))))


(defun test () ;NOTE see docstring.
  "Unit tests if the output matches.
Greatly diminishing the tests value, it does not test &rest, &key, \
&optional."
  (let ((rnd-args (loop repeat 50 collect (vector (gensym) (rnd))))
	(fault-cnt 0))
    (multiple-value-bind (args input)
	(split-ends (random-arg-list rnd-args)
		    (lambda (el) (values (aref el 0) (aref el 1))))
      (do-by-argument args input
	(lambda (val arg chain opt)
	  (declare (ignorable opt))
	  (unless (dolist (el rnd-args)
		    (when (eql (aref el 0) arg)
		      (return (= (aref el 1) val))))
	    (setf- + fault-cnt 1)))))
    (unless (= fault-cnt) fault-cnt)))

(loop repeat 500 append (test))

;;Playtests.

(do-by-argument '(a (&key m (n 1) o (p 5))  (b c &optional r (q 1)) &rest d)
  '(1 (:m 6 :p 13) (2 3 5) 4 5)
  (lambda (v a ch opt) (print (list v a ch opt))))

(macroexpand '(argumentize-list (a (x &key u v)
				 (b c &optional r (q 1)) &rest d)
	       list
  'body))

(do-by-argument '(a b (c e &key (lala 4) (bebe 5)) d &rest rest)
  '(1 2 (3 6 :bebe 1) 4 6 6) (lambda (el a d o)
		       (print (list el a d o))))
