(define (ad-eval form env)
  (cond ((symbol? form)
	 (ad-lookup form env))
	((pair? form)
	 (cond ((eq? (car form) 'quote)
		(scheme-value->ad-eval-value (cadr form)))
	       ((eq? (car form) 'lambda)
		(eval-ad-lambda form env))
	       ((eq? (car form) 'define)
		(eval-ad-definition form env))
	       ((eq? (car form) 'if)
		(eval-ad-if form env))
	       (else
		(eval-ad-application form env))))
	(else
	 (scheme-value->ad-eval-value form))))

(define (eval-ad-application form env)
  (let ((procedure (ad-eval (car form) env))
	(arguments (map (lambda (subform)
			  (ad-eval subform env))
			(cdr form))))
    (ad-apply procedure arguments)))

(define (ad-apply procedure arguments)
  (cond ((ad-primitive? procedure)
	 (apply (ad-primitive-implementation procedure) arguments))
	((ad-compound? procedure)
	 (ad-eval (ad-compound-body procedure)
		  (ad-extend-environment
		   (ad-compound-env procedure)
		   (ad-compound-formals procedure)
		   arguments)))
	(else
	 (error "Not an ad-procedure" procedure))))

(define (eval-ad-if form env)
  (let ((predicate (cadr form))
	(consequent (caddr form))
	(alternate (cadddr form)))
    (if (ad-eval predicate env)
	(ad-eval consequent env)
	(ad-eval alternate env))))

(define (eval-ad-lambda form env)
  (let ((formals (cadr form))
	(body (caddr form))) ; TODO Only one-form bodies in ad-lambda
    (make-ad-compound body env formals)))

(define (eval-ad-definition form env)
  (error "TODO definitions not supported in ad-eval yet"))

;;; Driver loop

(define ad-user-environment #f)

(define (run-ad)
  (if (not ad-user-environment)
      (set! ad-user-environment (make-ad-user-environment)))
  (display ";;; Ad> ")
  (let ((answer (ad-eval (read) ad-user-environment)))
    (display ";;; Ad value: ")
    (write answer)
    (newline))
  (run-ad))
