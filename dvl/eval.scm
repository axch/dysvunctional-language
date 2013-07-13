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
;;;; Concrete evaluator for DVL

;;; Functions take only one argument (which they may destructure
;;; inside).  CONS is a special form.  LAMBDA and CONS are the only
;;; non-macro special forms.  IF macroexpands into the only primitive
;;; procedure that accepts and calls DVL procedures as arguments.
;;; DVL-VALUE->SCHEME-VALUE suffices to let it work its magic properly
;;; in the concrete evaluator.

(define (concrete-eval exp env world win)
  (cond ((constant? exp) (win (constant-value exp) world))
        ((variable? exp) (win (lookup exp env) world))
        ((null? exp) (win '() world))
        ((lambda-form? exp)
         (win (make-closure exp env) world))
        ((pair-form? exp)
         (concrete-eval (car-subform exp) env world
          (lambda (car-value post-car-world)
            (concrete-eval (cdr-subform exp) env post-car-world
             (lambda (cdr-value new-world)
               (win (cons car-value cdr-value) new-world))))))
        ((application? exp)
         (concrete-eval (operator-subform exp) env world
          (lambda (operator post-operator-world)
            (concrete-eval (operand-subform exp) env post-operator-world
             (lambda (operand new-world)
               (concrete-apply operator operand new-world win))))))
        (else
         (syntax-error "Invalid expression type" exp))))

(define (concrete-apply proc arg world win)
  (cond ((primitive? proc)
         (apply-primitive proc arg world win))
        ((closure? proc)
         (concrete-eval (closure-body proc)
                        (extend-env (closure-formal proc)
                                    arg
                                    (closure-env proc))
                        world
                        win))
        (else
         (error "Invalid procedure type" proc))))

(define (apply-primitive proc arg world win)
  ((primitive-implementation proc) arg world win))

(define (interpret form)
  (concrete-eval
   (macroexpand form) (initial-user-env) (initial-world)
   (lambda (value world)
     value)))

(define (dvl-eval form)
  (concrete-eval
   (macroexpand form) (initial-user-env) (initial-world)
   (lambda (value world)
     (pp world)
     value)))
