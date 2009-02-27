(in-package #:lang)

(setf (fun-state-manual-type-coarser *state* '|var|)
      (lambda (general specific state vars)
	"Values are just tags, they don't matter if you're not looking for \
them."
	(flet ((is-value (tp)
		 (and* (listp tp) (case (car tp) (|var| t)))))
	  (cond
	    ((is-value general)
	     (unless (is-value specific) ;If both value, they match anyway.
	       (type-coarser (cadr general) compare-type
			     :state state :vars vars)))
	    ((is-value specific)
	     (type-coarser general (cadr specific)
			   :state state :vars vars))))))
