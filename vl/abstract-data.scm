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

;;; ANALYSIS-GET is \bar E_1 from the paper.
(define (analysis-get exp env analysis)
  (analysis-lookup exp env analysis
   (lambda (value)
     value)
   (lambda ()
     abstract-all)))

(define (same-analysis-binding? binding1 binding2)
  (abstract-equal? binding1 binding2))

(define (same-analysis? ana1 ana2)
  (lset= same-analysis-binding? (analysis-bindings ana1)
	 (analysis-bindings ana2)))
