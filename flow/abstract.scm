(define (abstract-eval-once exp abstract-env analysis)
  (analysis-lookup exp abstract-env analysis
   (lambda (value)
     value)
   (lambda ()
     abstract-all)))

(define (abstract-apply proc arg analysis)
  (cond ((primitive? proc)
	 ((primitive-abstract-implementation proc) arg))
	((closure? proc)
	 (if (abstract-all? arg)
	     abstract-all
	     (abstract-eval-once (closure-body closure)
				 (extend-abstract-env
				  (closure-formal closure)
				  arg
				  (closure-env closure))
				 analysis)))
	((abstract-all? proc)
	 abstract-all)
	(else
	 (error "Trying to abstractly apply something that is known not to be a procedure" proc arg analysis))))

(define (abstract-eval exp abstract-env analysis)
  (cond ((variable? exp)
	 (lookup exp abstract-env))
	((null? exp)
	 '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(make-abstract-closure (cadr exp) (caddr exp) abstract-env))
	       ((eq? (car exp) 'cons)
		(let ((car-answer (abstract-eval-once
				   (cadr exp) abstract-env analysis))
		      (cdr-answer (abstract-eval-once
				   (caddr exp) abstract-env analysis)))
		  (if (and (not (abstract-all? car-answer))
			   (not (abstract-all? cdr-answer)))
		      (cons car-answer cdr-answer)
		      abstract-all)))
	       (else
		(abstract-apply
		 (abstract-eval-once (car exp) abstract-env analysis)
		 (abstract-eval-once (cdr exp) abstract-env analysis)
		 analysis))))
	(else
	 (error "Invalid expression in abstract evaluator"
		exp abstract-env analysis))))



(define (analyze program)
  (let* ((env (env->abstract-env (initial-flow-user-env)))
	 (initial-analysis
	  (make-analysis
	   `((,program ,env ,abstract-all)))))
    (abstract-eval (macroexpand program) env initial-analysis)))
