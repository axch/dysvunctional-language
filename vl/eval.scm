;;; Functions take only one argument (which they may destructure
;;; inside).  Constants are (notionally) converted to variables and
;;; (notionally) looked up in environments.  CONS is a special form.
;;; LAMBDA, CONS, and LETREC are the only non-macro special forms.

;;; The evaluation structure is built out of MIT Scheme pairs.  These
;;; need to be kept distinct from the pairs that the object-language
;;; cons macro makes.

(define (concrete-eval exp env)
  (cond ((variable? exp)
	 (lookup exp env))
	((null? exp)
	 '())
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
			(extend-env (closure-env proc)
				    (closure-formal proc)
				    arg)))
	(else
	 (error "Invalid procedure type" proc))))

(define (apply-primitive proc arg)
  ((primitive-implementation proc) (flow-value->scheme-value arg)))
