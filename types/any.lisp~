
(in-package #:lang)

(add-manual-type-generality *state*
  (lambda (type compare-type state)
    (when (listp type)
      (eql (car type) '|any|))))