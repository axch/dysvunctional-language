;;;; Concrete evaluator for VL

;;; Functions take only one argument (which they may destructure
;;; inside).  CONS is a special form.  LAMBDA and CONS are the only
;;; non-macro special forms.  IF becomes a slightly magical primitive,
;;; but VL-VALUE->SCHEME-VALUE suffices to let it work its magic
;;; properly in the concrete evaluator.

(define (concrete-eval exp env)
  (cond ((constant? exp) exp)
	((variable? exp) (lookup exp env))
	((null? exp) '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(make-closure (cadr exp) (caddr exp) env))
	       ((eq? (car exp) 'cons)
		(cons (concrete-eval (cadr exp) env)
		      (concrete-eval (caddr exp) env)))
	       (else
		(concrete-apply (concrete-eval (car exp) env)
				(concrete-eval (cadr exp) env)))))
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
