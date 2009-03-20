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

;;TODO update doc strings, and comments.
;;TODO state is passed on everywhere, is it good? Remember, namespace has to
;;work too!

(in-package #:lang)

;'equally' specific is along in the list of typesets(which might be the more
; specific of some other typeset), more specific in the slot.
(defclass typeset ()
  ((more-specific :initarg :more-specific
		  :initform nil)
   (arg-types :initarg :arg-types :initform nil :accessor arg-types)))

(defclass top-typeset (typeset)
  ((exact-hash :initform (make-hash-table :test 'equalp))))

(defun type-list-prefer (type-a type-b &key state (lvl 0) no-conversion)
  "Which type is preferred, besides coarseness."
  (let (undecided done)
    (iter (for tp-a in type-a)
          (for tp-b in type-b)
       (let ((coarser-ab (type-coarser tp-a tp-b
			      :state state :no-conversion no-conversion))
	     (coarser-ba (type-coarser tp-b tp-a
			      :state state :no-conversion no-conversion)))
	 (when (cond
		 ((and coarser-ab coarser-ba)
		  nil)
		 ((not (or coarser-ab coarser-ba))
		  (push (list tp-a tp-b) undecided)
		  nil)
		 (t
		  (setf done t)))
	;Prefer those that are more specific earlier.
	   (return (cond (coarser-ab nil) (coarser-ba t)))))
       (finally
	(unless done
	  (dolist (u undecided) ;TODO type-list-prefer based on type-prefer.
	    (if (eql (caar u) (caadr u))
		(type-list-prefer (cdar u) (cdadr u) :state state
			:lvl (+ lvl 1) :no-conversion no-conversion)
		(string-lessp (caar u) (caadr u)))))))))

(defun type-prefer (type-a type-b &key state no-conversion)
  "Wrapped around type-list-prefer. (Shouldn't it be the other way around?)"
  (type-list-prefer (list type-a) (list type-b)
    :state state :no-conversion no-conversion))

(defun typeset-coarser (typeset specific &key (state *state*) no-conversion)
  "Does type-list-coarser for arg-types of typesets."
  (type-list-coarser (arg-types typeset) (arg-types specific)
		     :state state :no-conversion no-conversion))

(defun typeset-eql (typeset-a typeset-b &key (state *state*))
  "Does type-list-eql for arg-types of typesets."
  (type-list-eql (arg-types typeset-a) (arg-types typeset-b) :state state))

(defun typeset-prefer (ts-a ts-b &key state no-conversion)
  "Does type-list-prefer for arg-types of typesets."
  (type-list-prefer (arg-types ts-a) (arg-types ts-b)
		    :state state :no-conversion no-conversion))

(defun typeset-redivide (typeset to-ts &key (state *state*) no-conversion)
  "Checks all the slots in the typeset, and moves them to-ts if to-ts is
general enough. DOES NOT resort the thing."
  (with-slots (more-specific) typeset
    (let ((divide-list more-specific))
      (setf more-specific nil)
      (dolist (ts divide-list)
	(if (typeset-coarser to-ts ts
	      :state state :no-conversion no-conversion)
	  (push ts (slot-value to-ts 'more-specific))
	  (push ts more-specific))))))

(defun type-list-spec-string (arg-types)
  "Provides specifying string which is used to match directly via a hash\
 table."
  (format nil "~D" arg-types))
;TODO i suppose this is done consistently at all times! Am i correct?

(defun typeset-get (typeset arg-types &key (state *state*)
		    (top t) (use-hash t) no-conversion)
  "Gets a function based on the argument types."
  (cond
    ((and* top use-hash (equal (type-of typeset) 'top-typeset)
	  (gethash (type-list-spec-string arg-types)
		   (slot-value typeset 'exact-hash)))
     (gethash (type-list-spec-string arg-types)
	      (slot-value typeset 'exact-hash)))
    (t
     (dolist (ts (slot-value typeset 'more-specific))
      ;Look for the one that matches.
	(when (type-list-coarser (arg-types ts) arg-types
				 :state state :no-conversion no-conversion)
      ;When found, look if it has a more specific match.
	  (return (if-use (typeset-get ts arg-types :state state :top nil)
			  ts)))))))

;TODO eventually i will need to re-evaluate this function, it is very long..
(defun (setf typeset-get) (set-to typeset arg-types &key (state *state*)
			   (top t) (use-hash t) no-conversion)
  "Sets/creates a function based on the argument types."
  (cond
    ((when (and top use-hash (equal (type-of typeset) 'top-typeset))
       (gethash (type-list-spec-string arg-types)
		(slot-value typeset 'exact-hash)))
     (cond
       ((null set-to)
	nil) ;TODO figure out how to remove stuff.
       (t ;Found exact match.
	(setf (gethash (type-list-spec-string arg-types)
		       (slot-value typeset 'exact-hash))
	      set-to))))
    ((unless top
       (type-list-coarser arg-types (arg-types typeset)
			  :state state :no-conversion no-conversion))
     (error "(langs fault)Shouldn't be more general then the typeset you\
 are trying to add to."))
    ((dolist (ts (slot-value typeset 'more-specific))
       (when
	   (let (was-coarser
		 (coarser (type-list-coarser arg-types (arg-types ts)
			   :state state :no-conversion no-conversion))
		 (finer   (type-list-coarser (arg-types ts) arg-types
			   :state state :no-conversion no-conversion)))
	     (cond 
	     ;Match.
	       ((and finer coarser)
		(unless (type-list-eql arg-types (arg-types ts):state state)
		  (error (format nil "(langs fault)Finer and coarser, but\
 not equal ~D ~D" arg-types (arg-types ts))))
		(cond
		  ((null set-to)
		   nil) ;TODO figure out how to remove stuff.
		  (t
		   (setf (slot-value set-to 'more-specific)
			 (slot-value ts 'more-specific))
		   (setf ts set-to))))
 	      ;More general need to check if it is more general to other 
	      ; stuff here, and put those in the more specific of this one.
	       (coarser
		(setf was-coarser t)
		nil)
	     ;More specific then ts.
	       (finer
		(when was-coarser
		  (error "(langs fault)Error, shouldn't be more specific in\
 one way and less in another."))
		(if-use
		 (setf (typeset-get ts arg-types :state state :top nil)
		       set-to)
		 (unless (null set-to)
		   (error "(langs fault)If more general, always go down \
there."))))))
	 (return t)))
     set-to)
   ;Must be more specific then typeset but none of the other more specific
   ; are more general then it, so it should go in the more-specific of 
   ; typeset.
    (t
     ;Put things more specific in set-to.
     (typeset-redivide typeset set-to :state state)
     (flet ((re-sort (list)
	      (sort list (lambda (ts-a ts-b)
			   (typeset-prefer ts-a ts-b
			     :state state :no-conversion no-conversion)))))
       (with-slots (more-specific) typeset
     ;Find things that are more specific then what we have.
	 (dolist (ts more-specific)
	   (when-with prefer (typeset-prefer set-to ts
			       :state state :no-conversion no-conversion)
	     (unless (integerp prefer) ;Integers are choosen alphabetically.
	       (typeset-redivide ts set-to)
	       (setf- re-sort (slot-value ts 'more-specific)))))
     ;Now to add set-to in the correct place.
	 (push set-to more-specific)
	 (setf- re-sort more-specific))
       (setf- re-sort (slot-value set-to 'more-specific)))
     set-to)))

(defun typeset-add (typeset added &key (state *state*) (use-hash t))
  "Looks into the argument types for you. Uses (setf typeset-get)"
  (setf (typeset-get typeset (arg-types added)
		     :state state :use-hash use-hash)
	added))

(defun typeset-remove (typeset arg-types &key (state *state*))
  "Removes whatever matches exactly. Uses (setf typeset-get)
WARNING TODO not implemented actually removing yet."
  (setf (typeset-get typeset arg-types state state) nil))
