
(in-package #:lang)

(defclass interval ()
  ((fr :initform 0 :initarg :fr :type number :accessor fr)
   (to :initform 0 :initarg :to :type number :accessor to)))

(defun mk-interval (fr to)
  (make-instance 'interval :fr fr :to to))

(defgeneric add (a b))

(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a number) (b interval))
  (with-slots (fr to) b
    (mk-interval (+ a fr) (+ b to))))

(defmethod add ((a interval) (b number))
  (add b a))

(defmethod add ((a interval) (b interval))
  (mk-interval (+ (fr a) (fr b)) (+ (to a) (to b))))

(defgeneric multiply (a b))

(defmethod multiply ((a number) (b number))
  (* a b))

(defmethod multiply ((a number) (b interval))
  (with-slots (fr to) b
    (if (> a 0)
      (mk-interval (* a fr) (* a to))
      (mk-interval (* a to) (* a fr)))))

(defmethod multiply ((a interval) (b number))
  (multiply b a))

(defmethod multiply ((a interval) (b interval))
  (let ((ff (* (fr a) (fr b))) ;TODO this is the easy crude way.
	(ft (* (fr a) (to b)))
	(tf (* (to a) (fr b)))
	(tt (* (to a) (to b))))
    (mk-interval (min ff ft tf tt) (max ff ft tf tt))))

(defgeneric divide (x))

(defmethod divide ((x number))
  (/ x))

(defun type-to-set (type)
  (case (car type)
    (|eql|    (cadr type))
    (|number| (mk-interval (cadr type) (caddr type)))))

(defun set-to-type (set)
  (cond
    ((numberp set)
     `(|eql| ,set))
    ((eql (type-of set) 'interval)
     `(|number| ,(fr set) ,(to set)))))

(defun op-number (types op)
  (if (null (cdr types))
    (car types)
    (op-number (cons (set-to-type
		      (funcall op (type-to-set (first (print types)))
			          (type-to-set (second types))))
		     (cddr types))
	       op)))

(fun-add *local* '+ :names '(:usual +)
  :out-type-fn (lambda (fun types)
		 (declare (ignored fun))
		 (set-to-type (op-number types #'add))))

(fun-add *local* '* :names '(:usual *)
  :out-type-fn (lambda (fun types)
		 (declare (ignored fun))
		 (set-to-type (op-number types #'multiply))))

(fun-add *local* '- :names '(:usual -)
  :out-type-fn
  (lambda (fun types)
    (declare (ignored fun))
    (set-to-type
     (if (null (cdr types))
	 (multiply -1 (type-to-set (car type)))
	 (add (type-to-set (car type))
	      (multiply -1 (op-number (cdr type) #'add)))))))

;(fun-add *local* '/ :names '(:usual /)
;  :out-type-fn
;  (lambda (fun types)
;    (declare (ignored fun))
;
;(defgeneric sqr (x))
;
;(defmethod sqr ((x number))
;  (* x x))
;
;(defmethod sqr ((x interval))
;  (with-slots (fr to) x
;    (let ((fr2 (sqr fr))
;	  (to2 (sqr to)))
;      (if (> fr 0)
;	(mk-interval fr2 to2)
;	(if (< to 0)
;	  (mk-interval to2 fr2)
;	  (mk-interval 0 (max to2 fr2)))))))
;
;(fun-add *local* 'sqr
;  :args-code '(x) :body-code '(* x x)
;  :out-type-fn (lambda (fun types)
;		 (declare (ignored fun))
;		 (set-to-type (sqr (type-to-set (car types))))))

