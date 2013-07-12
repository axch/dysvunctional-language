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
;;;; Concrete evaluator for VL

;;; Functions take only one argument (which they may destructure
;;; inside).  CONS is a special form.  LAMBDA and CONS are the only
;;; non-macro special forms.  IF macroexpands into the only primitive
;;; procedure that accepts and calls VL procedures as arguments.

(define (concrete-eval exp env)
  (cond ((constant? exp) (constant-value exp))
        ((variable? exp) (lookup exp env))
        ((lambda-form? exp)
         (make-closure exp env))
        ((pair-form? exp)
         (cons (concrete-eval (car-subform exp) env)
               (concrete-eval (cdr-subform exp) env)))
        ((application? exp)
         (concrete-apply (concrete-eval (operator-subform exp) env)
                         (concrete-eval (operand-subform exp) env)))
        (else
         (error "Invalid expression type" exp))))

(define (concrete-apply proc arg)
  (cond ((primitive? proc)
         (apply-primitive proc arg))
        ((closure? proc)
         (concrete-eval (closure-body proc)
                        (extend-env (closure-formal proc)
                                    arg
                                    (closure-env proc))))
        (else
         (error "Invalid procedure type" proc))))

(define (apply-primitive proc arg)
  ((primitive-implementation proc) arg))

(define (interpret form)
  (concrete-eval (macroexpand form) (initial-user-env)))

