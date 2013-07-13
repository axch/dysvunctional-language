;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;;; Stalin runtime system

;;; Here is the complement of definitions that needs to be loaded into
;;; Stalin in order to compile FOL code.

(define (real x)
  x)

(define read-real read)

(define (write-real x)
  (write x)
  (newline)
  x)

(define *the-gensym* 0)

(define (gensym)
  (set! *the-gensym* (+ *the-gensym* 1))
  (- *the-gensym* 1))

(define (gensym= gensym1 gensym2)
  (= gensym1 gensym2))

(define values list)

(define (call-with-values values receiver)
  (apply receiver values))
