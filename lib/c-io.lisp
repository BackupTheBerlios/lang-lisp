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

(with-slots (write-namespace namespaces load-lib-actions) *state*
  (setf write-namespace '|c-io|)
  (setf namespaces nil)
  (setf (gethash |c-io| load-lib-actions)
	(lambda (type-of state)
	  )))

;TODO ok, so this one goes more general then string, and conversion will
;do rest.
(add-type '|c-string| ('atomic-type)
  :c-name '|char*| :size 8)

;;TODO make.
(fun-add '|c-remove| '((|ptr| (|char|))) :c-name '|rename|
	 :out-type '(int))
(fun-add '|c-rename| '((|ptr| (|char|)) (|ptr| (|char|))) :c-name '|rename|
	 :out-type '(int))


;FILE * tmpfile ( void );
;char * tmpnam ( char * str );
;int fclose ( FILE * stream );
;int fflush ( FILE * stream );
;FILE * fopen ( const char * filename, const char * mode );
;FILE * freopen ( const char * filename, const char * mode, FILE * stream );
;void setbuf ( FILE * stream, char * buffer );
;int setvbuf ( FILE * stream, char * buffer, int mode, size_t size );
;int fprintf ( FILE * stream, const char * format, ... );
;int fscanf ( FILE * stream, const char * format, ... );
;int printf ( const char * format, ... );
;int  scanf ( const char * format, ... );
;int sscanf ( const char * str, const char * format, ...);
;int vfprintf ( FILE * stream, const char * format, va_list arg );
;int vprintf ( const char * format, va_list arg );
;