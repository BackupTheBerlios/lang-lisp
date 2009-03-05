(in-package #:umac)

(def-umac loop (:inc 1) (&rest series)
  "Loops a series until (end ',loop-name) is called."
  (let ((loop-name (if (symbolp (car series)) (car series) (gensym))))
    (set-end-name loop-name)
    (series-stuff loop-name (`(umac-is-end ,loop-name) nil)
		  (if (symbolp (car series)) (cdr series) series))))

(def-umac repeat () (count &rest series)
  "Repeat something some number of times. If count a list, it is the 
counting index, upto-value and then the name you can umac-end-to with."
  (argumentize-list (i upto repeat-name)
      (if (listp count) count (list (gensym) count))
    (setf repeat-name (if-use repeat-name (gensym)))
    `(loop ,repeat-name
	(while (< ,i ,upto))
	(series-no-inc ,repeat-name ,@series)
	(sum i 1))))

(def-umac for (:pass-first t) (name start change)
  (when (eql change name)
    (error "For is only for changing variables. (Use var)"))
  (add-var `(,name ,start))
  `(do (setf ,name ,change)))

(def-umac for-on-list (:pass-first t) (el list key)
  (add-var `(,name ,start))
  (case key
    (:while
     `(body
	(setf ,el (cdr ,el))
	(when (null ,el)
	  (return nil))))
    (nil
     `(setf ,el (cdr ,el)))
    (t
     (error "You gave a bad key for umac for-on-list."))))

(def-umac for-range (:pass-first t) (i from to incr)
  (add-var `(i ,from))
  `(progn
     (setf ,i (+ ,i ,(if-use incr 1)))
     (when (>= ,i ,to) (return))))
