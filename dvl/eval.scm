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
         (error "Invalid expression type" exp))))

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

(define (dvl-eval form #!optional suppress-print-world?)
  (concrete-eval
   (macroexpand form) (initial-user-env) (initial-world)
   (lambda (value world)
     (if (or (default-object? suppress-print-world?)
             (not suppress-print-world?))
         (pp world))
     value)))
