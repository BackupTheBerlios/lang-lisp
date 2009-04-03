
;;Note that this file assumes that base directory is ../
;;

;Load it.
(load "other/generic.lisp")
(load "convert/xml.lisp")
(load "other/namespace.lisp")
(load "other/simple-macexpand.lisp")

(defpackage #:play
  (:use #:common-lisp #:generic #:xml-out #:simple-macexpand))

(in-package #:play)

;;You can convert a nested list to an xml text
;   * If the second element of a list is an 
;(outputting to print)
(produce-xml-stream '(a (attr (q "314") (r "1")) b c) nil)

;Just an example, obviously will need a better macro for it.
(add-mac 'img (&key alt src) (:code-var code)
  (values `(img (attr ,@(when alt `((alt ,alt))) ,@(when src `((src ,src)))))
	  :stop))

(produce-xml-stream (resolve '(img src "not-a-lolcat.png"))
		    nil)
