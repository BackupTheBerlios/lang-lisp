
(in-package #:lang)

(defun treat-type (type)
  "Cleans up a type, removes redundant parts, simplifies."
  (when (eql (car type) '|or|)
    (setf type `(|or| ,@(treat-type-or (cdr type)))))
  (setf type `(|or| ,@(treat-type-numeric-or (cdr type))))
  (if (= (length type) 2)
    (cadr type) type))

(defun treat-type-or (types)
  "Removes all sub-ors."
  (iter (for tp in types)
	(if (eql '|or| (car tp))
	  (appending (treat-type-or (cdr tp)))
	  (collect tp))))

(defun inside-numtype (tp x)
  "Returns if a single position is inside a numtype."
  (case (car tp)
    (|eql|    (when (= (cadr tp) x) tp))
    (|number| (when (<= (cadr tp) x (caddr tp)) tp))
    (|or|     (dolist (type (cdr tp))
		(when (inside-numtype type x)
		  (return type))))))

(defun join-type-numeric (a b)
  "Returns nil if the two are separate, the new joined type if they're not."
  (case (car a)
    (|eql|
     (inside-numtype b (cadr a)))
    (|number|
     (flet ((in-range (rl x)
	      (<= (cadr rl) x (caddr rl))))
       (case (car b)
	 (|eql|
	  (when (in-range a (cadr b))
	    a))
	 (|number|
	  (when (or (in-range a (cadr b)) (in-range a (caddr b))
		    (in-range b (cadr a)) (in-range b (caddr a)))
	    `(|number| ,(min (cadr a) (cadr b))
		       ,(max (caddr a) (caddr b))))))))))

(defun treat-type-numeric-or (types)
  "Removes overlap in orred intervals."
  (if (null(cdr types))
    types
    (let ((joined (car types)) (altered t)
	  (unjoined (cdr types)))
      (do () ((not altered) nil)
	(setf altered nil)
	(setf unjoined
	      (iter (for tp in unjoined)
		    (if-with new-joined (join-type-numeric joined tp)
		      (progn (setf altered t)
			     (setf joined new-joined))
		      (collect tp)))))
      (cons joined
	    (treat-type-numeric-or unjoined)))))

(defun basic-op (op ta tb &key (up/down-1 t) up/down-2)
  "Basic operation for numeric '|eql|, |number|."
  (case (car ta)
    (|eql|
     (case (car tb)
       (|eql|
	`(|eql| ,(funcall op (cadr ta) (cadr tb))))
       (|number|
	(when up/down-1 ;This not always applicable; function must either 
	                ;rise or fall when one var held constant.
	  (let ((fr (funcall op (cadr ta) (cadr tb)))
		(to (funcall op (cadr ta) (caddr tb))))
	    `(|number| ,(min fr to) ,(max fr to)))))))
    (|number|
     (case (car tb)
       (|eql|
	(basic-op op tb ta :up/down-1 up/down-1 :up/down-2 up/down-2))
       (|number|
	(when up/down-2 ;Applicable when corners are maximum/minimum.
	  (argumentize-list (f-a t-a) (cdr ta)
	  (argumentize-list (f-b t-b) (cdr tb)  
	    (let ((ff (funcall op f-a f-b)) (ft (funcall op f-a t-b))
		  (tf (funcall op t-a f-b)) (tt (funcall op t-a t-b)))
	      `(|number| ,(min ff ft tf tt) ,(max ff ft tf tt)))))))))
    (|or|
     (treat-type
      `(|or| ,@(iter
		(for tp in (cdr ta))
		(collect
		    (basic-op op tp tb
		      :up/down-1 up/down-1 :up/down-2 up/down-2))))))))

(defun types-+ (ta tb)
  (basic-op #'+ ta tb :up/down-2 t))

(defun types-* (ta tb)
  (basic-op #'* ta tb :up/down-2 t))

(defun to-type-list-via-bin (fn)
  (lambda (types)
    (do ((types (cdr types) (cdr types))
	 (tp (car types)
	     (funcall fn tp (car types))))
	((null types) tp))))
  
(defun to-out-type-fn-via-bin (fn)
  (to-out-type-fn (to-type-list-via-bin fn)))

(defun to-out-type-fn (fn)
  (lambda (fun types)
    (declare (ignored fun))
    (funcall fn types)))

(fun-add *local* '+ :names '(:usual +)
  :out-type-fn (to-out-type-fn-via-bin #'types-+))
(fun-add *local* '* :names '(:usual *)
  :out-type-fn (to-out-type-fn-via-bin #'types-*))

(defun types-- (tp)
  (case (car tp)
    (|eql|
     `(|eql| ,(- (cadr tp))))
    (|number|
     `(|number| ,(- (caddr tp)) ,(- (cadr tp))))
    (|or|
     (treat-type
      `(|or| ,@(iter
		(for tp in (cdr tp))
		(collect (types-- tp))))))))

(defun types-/ (tp)
  (case (car tp)
    (|eql|
     `(|eql| ,(/ tp)))
    (|number|
     (cond
       ((< (cadr tp) 0 (caddr tp))
	`(or (|number| nil ,(/ (cadr tp)))
	     (|number| ,(/ (caddr tp)))))
       ((= (cadr tp) 0)
	(if (> (caddr tp) 0)
	  `(|number| ,(/ (caddr tp)))
	  `(|number| nil ,(/ (caddr tp)))))
       ((= (caddr tp) 0)
	(if (> (cadr tp))
	  `(|number| nil ,(/ (cadr tp)))
	  `(|number| ,(/ (cadr tp)))))
       (t
	`(|number| ,(/ (caddr tp)) ,(/ (cadr tp))))))
    (|or|
     (treat-type
      `(|or| ,@(iter
		(for tp in (cdr tp))
		(collect (types-/ tp))))))))


(defun to-type-list-inverse-bin (inverse fn)
  (lambda (types)
    (if (null (cdr types))
      (funcall inverse (car types))
      (funcall fn (car types)
	          (funcall inverse
			   (funcall (to-type-list-via-bin fn)
				    (cdr types)))))))

(defun to-out-type-fn-inverse-bin (inverse fn)
  (to-out-type-fn (to-type-list-inverse-bin inverse fn)))

(fun-add *local* '- :names '(:usual -)
  :out-type-fn (to-out-type-fn-inverse-bin #'types-- #'types-+))

(fun-add *local* '/ :names '(:usual /)
  :out-type-fn (to-out-type-fn-inverse-bin #'types-/ #'types-*))


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

