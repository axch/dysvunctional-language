(define (abstract-lookup exp env)
  (if (constant? exp)
      exp
      (let ((answer (assq exp (env-bindings env))))
	(if answer
	    (cdr answer)
	    (error "Variable not found" exp env)))))

(define (env-slice env symbols)
  (define (restrict-bindings bindings)
    (filter (lambda (binding)
	      (memq (car binding) symbols))
	    bindings))
  (if (env? env)
      (make-env (restrict-bindings (env-bindings env)))
      (make-env (restrict-bindings (env-bindings env)))))

(define (interesting-variable? env)
  (lambda (var)
    (not (solved-abstractly? (abstract-lookup var env)))))

(define-structure (analysis (safe-accessors #t))
  bindings)

(define (analysis-lookup exp env analysis win lose)
  (let loop ((bindings (analysis-bindings analysis)))
    (if (null? bindings)
	(lose)
	(if (and (equal? exp (caar bindings))
		 (abstract-equal? env (cadar bindings)))
	    (win (caddar bindings))
	    (loop (cdr bindings))))))

(define (same-analysis-binding? binding1 binding2)
  (abstract-equal? binding1 binding2))

(define (same-analysis? ana1 ana2)
  (lset= same-analysis-binding? (analysis-bindings ana1)
	 (analysis-bindings ana2)))
