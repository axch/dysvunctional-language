;;; Given a particular abstract analysis, the trio REFINE-EVAL,
;;; REFINE-APPLY, and REFINE-EVAL-ONCE will convert an expression
;;; and an abstract environment into an abstract value.  This is
;;; (possibly) a refinement of the knowledge in the analysis.

;;; In other words, given the available knowledge about what various
;;; expressions evaluate to in various environments, these three will
;;; evaluate one expression in one environment to a depth of about 1,
;;; and maybe produce better knowledge about what that expression
;;; evaluates to in that environment, which can then be incorporated
;;; into an updated collection of available knowledge.

(define (refine-eval-once exp abstract-env analysis)
  (analysis-lookup exp abstract-env analysis
   (lambda (value)
     value)
   (lambda ()
     abstract-all)))

(define (refine-apply proc arg analysis)
  (cond ((primitive? proc)
	 (if (eq? proc primitive-if)
	     ((primitive-abstract-implementation proc) arg analysis)
	     ((primitive-abstract-implementation proc) arg)))
	((closure? proc)
	 (if (abstract-all? arg)
	     abstract-all
	     (refine-eval-once (closure-body proc)
			       (extend-abstract-env
				(closure-formal proc)
				arg
				(closure-env proc))
			       analysis)))
	((abstract-all? proc)
	 abstract-all)
	(else
	 (error "Trying to refine the application of something that is known not to be a procedure" proc arg analysis))))

(define (refine-eval exp abstract-env analysis)
  (cond ((constant? exp) exp)
	((variable? exp)
	 (abstract-lookup exp abstract-env))
	((null? exp) '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(make-closure (cadr exp) (caddr exp) abstract-env))
	       ((eq? (car exp) 'cons)
		(let ((car-answer (refine-eval-once
				   (cadr exp) abstract-env analysis))
		      (cdr-answer (refine-eval-once
				   (caddr exp) abstract-env analysis)))
		  (if (and (not (abstract-all? car-answer))
			   (not (abstract-all? cdr-answer)))
		      (cons car-answer cdr-answer)
		      abstract-all)))
	       (else
		(refine-apply
		 (refine-eval-once (car exp) abstract-env analysis)
		 (refine-eval-once (cadr exp) abstract-env analysis)
		 analysis))))
	(else
	 (error "Invalid expression in abstract refiner"
		exp abstract-env analysis))))

(define (refine-analysis-binding binding analysis)
  (let ((exp (car binding))
	(env (cadr binding)))
    (list (list exp env (refine-eval exp env analysis)))))

(define (refine-analysis analysis)
  (apply lset-union same-analysis-binding?
	 (map (lambda (binding)
		(refine-analysis-binding binding analysis))
	      (analysis-bindings analysis))))

;;; Given a particular abstract analysis, and a particular expression
;;; EXP and abstract environment ENV, the trio EXPAND-EVAL,
;;; EXPAND-APPLY, and EXPAND-EVAL-ONCE will detect the set of
;;; once-removed expression-environment pairs that would shed light on
;;; the abstract evaluation of EXP in ENV but have not yet been
;;; analyzed at all (and return that set as a list of abstract
;;; bindings of those pairs to the abstract top value).

;;; In other words, these three expand the scope of which
;;; expression-environment pairs it appears are worth analyzing.

(define (expand-eval-once exp abstract-env analysis)
  (analysis-lookup exp abstract-env analysis
   (lambda (value)
     '())
   (lambda ()
     (list (list exp abstract-env abstract-all)))))

(define (expand-apply proc arg analysis)
  (cond ((primitive? proc)
	 (if (or (abstract-all? arg) (not (eq? primitive-if proc)))
	     '()
	     (let ((predicate (car arg))
		   (consequent (cadr arg))
		   (alternate (cddr arg)))
	       (define (expand-thunk-application thunk)
		 (expand-eval-once
		  `(,(closure-expression thunk) ()) (closure-env thunk) analysis))
	       (if (not (abstract-boolean? predicate))
		   (if predicate
		       (expand-thunk-application consequent)
		       (expand-thunk-application alternate))
		   (lset-union same-analysis-binding?
			       (expand-thunk-application consequent)
			       (expand-thunk-application alternate))))))
	((closure? proc)
	 (if (abstract-all? arg)
	     '()
	     (expand-eval-once (closure-body proc)
			       (extend-abstract-env
				(closure-formal proc)
				arg
				(closure-env proc))
			       analysis)))
	((abstract-all? proc)
	 '())
	(else
	 (error "Trying to expand on an application of something that is known not to be a procedure" proc arg analysis))))

(define (expand-eval exp abstract-env analysis)
  (cond ((variable? exp) '())
	((null? exp) '())
	((constant? exp) '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		'())
	       ((eq? (car exp) 'cons)
		(lset-union
		 same-analysis-binding?
		 (expand-eval-once (cadr exp) abstract-env analysis)
		 (expand-eval-once (caddr exp) abstract-env analysis)))
	       (else
		(lset-union
		 same-analysis-binding?
		 (expand-eval-once (car exp) abstract-env analysis)
		 (expand-eval-once (cadr exp) abstract-env analysis)
		 (expand-apply
		  (refine-eval-once (car exp) abstract-env analysis)
		  (refine-eval-once (cadr exp) abstract-env analysis)
		  analysis)))))
	(else
	 (error "Invalid expression in abstract expander"
		exp abstract-env analysis))))

(define (expand-analysis-binding binding analysis)
  (let ((exp (car binding))
	(env (cadr binding)))
    (expand-eval exp env analysis)))

(define (expand-analysis analysis)
  (apply lset-union same-analysis-binding?
	 (map (lambda (binding)
		(expand-analysis-binding binding analysis))
	      (analysis-bindings analysis))))

(define (step-analysis analysis)
  (make-analysis
   (lset-union same-analysis-binding?
	       (refine-analysis analysis)
	       (expand-analysis analysis))))

(define *analyze-wallp* #f)

(define (analyze program)
  (let* ((program (macroexpand program))
	 (env (env->abstract-env (initial-vl-user-env)))
	 (initial-analysis
	  (make-analysis
	   `((,program ,env ,abstract-all)))))
    (let loop ((old-analysis initial-analysis)
	       (new-analysis (step-analysis initial-analysis))
	       (count 0))
      (if (and *analyze-wallp* (= 0 (modulo count *analyze-wallp*)))
	  (pp new-analysis))
      (if (same-analysis? old-analysis new-analysis)
	  new-analysis
	  (loop new-analysis (step-analysis new-analysis) (+ count 1))))))
