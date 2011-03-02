(declare (usual-integrations))
;;;; Concrete evaluator for VL

;;; Functions take only one argument (which they may destructure
;;; inside).  CONS is a special form.  LAMBDA and CONS are the only
;;; non-macro special forms.  IF macroexpands into the only primitive
;;; procedure that accepts and calls VL procedures as arguments.
;;; VL-VALUE->SCHEME-VALUE suffices to let it work its magic properly
;;; in the concrete evaluator.

(define (concrete-eval exp env)
  (cond ((constant? exp) (constant-value exp))
	((variable? exp) (lookup exp env))
	((lambda-form? exp)
	 (make-closure (lambda-formal exp) (lambda-body exp) env))
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
  ((primitive-implementation proc) (vl-value->scheme-value arg)))

(define (vl-eval form)
  (concrete-eval (macroexpand form) (initial-vl-user-env)))

;;;; Concrete REPL

(define vl-user-env #f)

(define (start-vl)
  (set! vl-user-env (initial-vl-user-env))
  (run-vl))

(define (run-vl)
  (display "vl > ")
  (let ((answer (concrete-eval (macroexpand (read)) vl-user-env)))
    (display "; vl value: ")
    (write answer)
    (newline))
  (run-vl))
