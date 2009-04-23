
(in-package #:lang)

;;Test of flattening or-lists
; (Bit a simple thing but test, but its done now..)
(defun gen-or (len deepen-chance max-depth
	       &key (content (lambda () (list (gensym)))))
  (iter (repeat len)
	(collect (if (and (> max-depth 0) (< (random 1.0) deepen-chance))
		   (gen-or len deepen-chance (- max-depth 1))
		   (funcall content)))))

(defun check-or (types)
  (dolist (tp types)
    (when (eql (car tp) '|or|)
      (error "Did not filter an or. ~D is in ~D" tp types))))

(defun test-treat-type-or
    (&key (cnt 10) (len 20) (max-depth 10) (deepen-chance 0.1))
  "Tests treat-type-or."
  (dotimes (k cnt)
    (check-or (treat-type-or (gen-or len deepen-chance max-depth)))))

(test-treat-type-or :len 4 :cnt 20) ;Works

;;Tests of joining numeric types.

;;Whether still any overlapping parts left.

(defun gen-numeric-or (len &key (eql-prob 0.4) (size-ratio 0.7)
			        (from -1.0) (to 1.0))
  (let ((wid (/ (* (- to from) size-ratio) len)))
    (print (* wid len))
    (iter
      (repeat len)
      (collect
	  (flet ((rnd-pos ()
		   (+ from (random (- to from)))))
	    (if (< eql-prob (random 1.0))
	      `(|eql| ,(rnd-pos))
	      (let ((fr (rnd-pos)))
		`(|number| ,fr ,(+ fr (* (+ 0.5 (random 0.5)) wid))))))))))

(defun check-numeric-or (types &key same-check)
  (let ((old-types types))
    (setf- treat-type-numeric-or types)
    (unless (= (length old-types) (length types))
      (error "Processing twice changed something:~%~D~%~D" old-types types))
    (when same-check
      (unless (equalp types old-types)
	(error "Processing twice changed something; does not necate bug,\
 but..~%~D~%~D" old-types types))))
  (iter
    (for it on types)
    (dolist (tp (cdr it))
      (when
	  (case (caar it)
	    (|eql|
	     (inside-numtype tp (cadar it)))
	    (|number|
	     (flet ((in-range (rl x)
		      (<= (cadr rl) x (caddr rl))))
	       (case (car tp)
		 (|eql|    (in-range (car it) (cadr tp)))
		 (|number| (or (in-range (car it) (cadr tp))
			       (in-range (car it) (caddr tp))
			       (in-range tp (cadar it))
			       (in-range tp (caddar it))))))))
	(error "Treating of numeric-ors failed, these still overlap:
 ~D~%~D"
	       (car it) tp)))))

(defun test-treat-type-numeric-or-redundant
    (cnt &key (len 10) (eql-prob 0.4) (size-ratio 0.7)
              (from -1.0) (to 1.0))
  "Tests treat-type-numeric-or, but only if it has redundancy."
  (dotimes (k cnt)
    (check-numeric-or (treat-type-numeric-or
		       (gen-numeric-or
			len :size-ratio size-ratio :from from :to to)))))

(test-treat-type-numeric-or-redundant 50) ;Works.

;Whether the sets match.

(defun test-treat-type-numeric-or-try
    (cnt hit-cnt &key (len 10) (eql-prob 0.4) (size-ratio 0.7)
	              (from -1.0) (to 1.0))
  "Tests treat-type numeric-or by seeing if they 'claim' the same elements."
  (dotimes (k cnt)
    (let*((before (gen-numeric-or
		   len :size-ratio size-ratio :from from :to to))
	  (after  (treat-type-numeric-or before)))
      (dotimes (j hit-cnt)
	(let*((pos (+ from (random (- to from))))
	      (b (inside-numtype `(|or| ,@before) pos))
	      (a (inside-numtype `(|or| ,@after) pos)))
	  (when (if b (not a) a)
	    (error "The try at ~D failed.~%~D vs ~D" pos b a)))))))

(test-treat-type-numeric-or-try 10 100) ;Works.
