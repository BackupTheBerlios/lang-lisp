
(in-package #:lang)

;;Playtesting.

(setf *default-conv* (list nil))

(def-conv ab (a b) (list 'ab input rest))

(def-conv nil (b c) (list 'bc input rest))

(def-conv nil (c d) (list 'cd input rest))

(def-conv nil (d e) (list 'de input rest))

(chain-convs '(a b c d e))

(evalf 'a 'e 'i 'r)

(get-conv 'a 'e)

(funcall (getf (getf (car *default-conv*) 'a) 'c) 1 2)

(print (car *default-conv*))