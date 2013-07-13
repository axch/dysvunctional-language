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

(declare (usual-integrations))
;;;; FOL Primitivies

(define-structure (primitive safe-accessors)
  name
  type
  pure?)

(define (primitive-impure? primitive)
  (not (primitive-pure? primitive)))

(define-structure
  (function-type
   (constructor function-type)
   safe-accessors
   (print-procedure
    (lambda (state object)
      (with-current-unparser-state state
        (lambda (port)
          (format port "(-> ~S ~S)"
                  (arg-types object) (return-type object)))))))
  args
  return)

(define return-type function-type-return)
(define arg-types function-type-args)

(define (real->real thing)
  (make-primitive thing (function-type '(real) 'real) #t))
(define (real*real->real thing)
  (make-primitive thing (function-type '(real real) 'real) #t))
(define (real->bool thing)
  (make-primitive thing (function-type '(real) 'bool) #t))
(define (real*real->bool thing)
  (make-primitive thing (function-type '(real real) 'bool) #t))

;; Type testers real? gensym? null? pair? procedure? have other types, but
;; should never be emitted by VL or DVL on union-free inputs.

(define *primitives*
  `(,(make-primitive 'read-real (function-type '() 'real) #f)
    ,(make-primitive 'write-real (function-type '(real) 'real) #f)
    ,@(map real->real
           '(abs exp log sin cos tan asin acos sqrt real))
    ,@(map real*real->real '(+ - * / atan expt))
    ,@(map real->bool '(zero? positive? negative?))
    ,@(map real*real->bool '(< <= > >= =))
    ,(make-primitive 'gensym (function-type '() 'gensym) #f)
    ,(make-primitive 'gensym= (function-type '(gensym gensym) 'bool) #t)
    ,(make-primitive 'gensym< (function-type '(gensym gensym) 'bool) #t)))
