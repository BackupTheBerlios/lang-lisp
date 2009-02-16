;;
;;  Copyright (C) 2009-02-07 Jasper den Ouden.
;;
;;  This file is part of Lang(working title).
;;
;;  Lang is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Lang is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with Lang.  If not, see <http://www.gnu.org/licenses/>.
;;
(in-package #:lang)

(defvar *ts* (make-instance 'typeset))

(setf (slot-value *state* 'manual-type-generality)
      nil)

(defun simple-test (typeset &key (count 1000)
		         (end-chance 0.3) (maxlen 5) (maxdepth 5))
  "Tests adding and getting the added items.
does NOT test:
* Getting something more specific then anything in there.
* Always getting nil if it does not exist yet.
* Anything done with (slot-value state 'manual-type-generality).
It returns the erronous ones, nil if none."
  (let ((added
	 (loop repeat count collect
	   (let ((arg-types
		  (random-tree end-chance maxlen maxdepth :top t)))
	     (typeset-add typeset (make-instance 'typeset
			  :arg-types arg-types) :use-hash nil)
	     arg-types))))
    (loop for a in added
       unless (when-with got (typeset-get typeset a)
		(type-list-eql (arg-types got) a))
       collect (let ((got (typeset-get typeset a :use-hash nil)))
		 `(:missed ,a ,(when got (arg-types got)) ,got
		    ,(brute-force-locate typeset a :top t)
		    ,(when got (brute-force-locate got a :top t)))))))

(defun brute-force-locate (typeset type &key top)
  (cond
    ((and (not top) (type-list-eql (arg-types typeset) type))
     typeset)
    (t
     (let (found)
       (loop for ts in (slot-value typeset 'more-specific)
	  until (setf found (brute-force-locate ts type)))
       found))))


(setf *err* (simple-test *ts* :maxlen 3 :maxdepth 3 :count 1000))

(car *err*)

(slot-value (nth 4 (car *err*)) 'arg-types)

(print 'a)

(slot-value (car(slot-value *ts* 'more-specific)) 'more-specific)

(when-with miss (car(simple-test *ts* :maxlen 3 :maxdepth 3))
  (argumentize-list (want-tps got-tps ts brute brutelow) (cdr miss)
    (print (list :got got-tps :want want-tps
		 :brute (when brute (arg-types brute))
		 :brutelow brutelow))
    (print(loop for el in (slot-value ts 'more-specific)
;	     when (type-list-coarser got-tps (arg-types el))
	     collect (arg-types el)))))

(defun typeset-correctness (typeset &key top (specificness t)
			    (level-specificness t) (preference t))
  "Test if the typeset is ordered properly."
  (with-slots (more-specific) typeset
    (append
     (unless top
       (loop for ts in more-specific
	  when (and
		specificness
		(type-list-coarser (arg-types ts) (arg-types typeset)))
	  collect `(:spec ,ts)))
     (when preference
       (loop for ts on more-specific
	  when (if (null (cdr ts)) nil
		   (typeset-prefer (car ts) (cadr ts) :state *state*))
	  collect `(:pref ,typeset ,ts)))
     (when level-specificness
       (loop for rest on more-specific append
	 (loop for ts in (cdr rest)
	    when (type-coarser (arg-types ts) (arg-types (car rest)))
	    collect `(:level-spec ,(car rest) ,ts)
	    when (type-coarser (arg-types (car rest)) (arg-types ts))
	    collect `(:level-spec ,ts ,(car rest)))))
     (loop for ts in more-specific
	append (typeset-correctness ts :specificness specificness
		 :preference preference
		 :level-specificness level-specificness)))))

(loop for a in (typeset-correctness *ts* :top t)
   when (eql (car a) :pref)
   collect
  (loop for ts on (slot-value (cadr a) 'more-specific)
     unless (null (cdr ts))
     collect (typeset-prefer (car ts) (cadr ts) :state *state*)))

;TODO preference check fails, but at least it does not ever fail finding 
;exact matches.
(length(typeset-correctness *ts* :top t))


(let ((g (cadr (typeset-correctness *ts* :top t))))
  (with-slots (more-specific) (cadr g)
    (flet ((prefer (ts-a ts-b)
	     (let ((ab (typeset-prefer ts-a ts-b :state *state*))
		   (ba (typeset-prefer ts-b ts-a :state *state*)))
	       (when (or (and ab ba)
			 (not (or ab ba)))
		 (format t ":pref-wrong ~D ~D" ab ba))
	       ab)))
    (flet ((re-sort (list)
	     (sort list #'prefer)))
;      (setf- re-sort more-specific)
      (loop for ts on more-specific
	 unless (null (cdr ts))
	 collect (prefer (car ts) (cadr ts)))))))
      ; collect `(:pref ,ts)))))


  (list (arg-types g))); (arg-types (caddr g))))

(type-list-eql '((a)) '((b)))

(type-list-coarser '(V (R (H O) D)) '((O (A A)) (R (H Q) (S G))))

(type-list-prefer '((a c) (b))  '((a c) (b)) :state *state*)


(loop for el on (sort (loop repeat 10
			 collect (format nil "~D" (random 1000)))
		      'string-lessp)
     unless (null (cdr el))
     collect (string-lessp (cadr el) (car el)))


