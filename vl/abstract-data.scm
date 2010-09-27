(define (interesting-variable? env)
  (lambda (var)
    (not (solved-abstractly? (lookup var env)))))

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
