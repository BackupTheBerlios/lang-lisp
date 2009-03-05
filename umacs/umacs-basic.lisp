(in-package #:umac)

(def-umac body () (&rest body)
  "Just runs the body."
  `(progn ,@body))

(def-umac var () (var-name value)
  "Creates variable."
  (add-var `(,var-name ,value))
  nil)

(def-umac var-list () (&rest var-list)
  "Creates multiple variables."
  (dolist (v var-list)
    (add-var v))
  nil)

(def-umac series-no-inc ()
		 (series-name &rest series)
  "Does umacs expression in series."
  (set-end-name series-name)
  `(progn ,@(loop for el in series collect
		 `(unless (umac-is-end ,series-name)
		    ,(umac- el)))))

(def-umac series (:inc 1)
		 (series-name &rest series)
  "Does umacs expression in series."
  (set-end-name series-name)
  `(progn ,@(loop for el in series collect
		 `(unless (umac-is-end ,series-name)
		    ,(umac- el)))))

(def-umac stop-when (:cnt-num cnt-num) (&rest conditions)
  "Stop when all conditions true."
  `(when (and ,@conditions)
     (umac-end-to ,(- cnt-num 1))))
(def-umac stop-unless (:cnt-num cnt-num) (&rest conditions)
  "Continue unless all conditions true."
  `(unless (and ,@conditions)
     (umac-end-to ,(- cnt-num 1))))

(def-umac while () (&rest conds)
  "Synonym stop-when."
  (umac- `(stop-unless conds)))

(def-umac until () (&rest conds)
  "Synonym stop-unless"
  (umac- `(stop-when conds)))

;;Collecting/summing/etc.
(def-umac collect () (to &rest collected)
  "Collects to variable."
  (add-var to)
  `(setf- append ,to (list ,@collected)))

(def-umac append () (to &rest appended)
  "Appends to variable."
  (add-var to)
  `(setf- append ,to ,@appended))

(def-umac sum () (to &rest summed)
  "Sums to variable."
  (add-var (if (listp to) to `(,to 0)))
  `(setf- + ,(if (listp to) (car to) to) ,@summed))

(def-umac funcall () (to fun &rest args)
  "Function-call changing."
  (add-var to)
  `(setf to (funcall fun to args)))

(def-umac operation () (to operation &rest args)
  "Some arbitrary operation applied to something."
  (add-var to)
  `(setf- ,operation ,to ,@args))
