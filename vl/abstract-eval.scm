;;; Given a particular abstract analysis, REFINE-EVAL and REFINE-APPLY
;;; will convert an expression and an (abstract) environment into an
;;; abstract value.  This is (presumably) a refinement of the knowledge
;;; in the analysis.

;;; In other words, given the available knowledge about what various
;;; expressions evaluate to in various environments, these two will
;;; evaluate one expression in one environment to a depth of about 1,
;;; and maybe produce better knowledge about what that expression
;;; evaluates to in that environment, which can then be incorporated
;;; into an updated collection of available knowledge.

;;; REFINE-APPLY is \bar A from the paper.
(define (refine-apply proc arg analysis)
  (cond ((primitive? proc)
	 (if (eq? proc primitive-if)
	     ((primitive-abstract-implementation proc) arg analysis)
	     ((primitive-abstract-implementation proc) arg)))
	((closure? proc)
	 (if (abstract-all? arg)
	     abstract-all
	     (analysis-get (closure-body proc)
			   (extend-env
			    (closure-formal proc)
			    arg
			    (closure-env proc))
			   analysis)))
	((abstract-all? proc)
	 abstract-all)
	(else
	 (error "Trying to refine an application of something that is known not to be a procedure"
		proc arg analysis))))

;;; REFINE-EVAL is \bar E from the paper.
(define (refine-eval exp env analysis)
  (cond ((constant? exp) exp)
	((variable? exp) (lookup exp env))
	((null? exp) '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(make-closure (cadr exp) (caddr exp) env))
	       ((eq? (car exp) 'cons)
		(let ((car-answer (analysis-get
				   (cadr exp) env analysis))
		      (cdr-answer (analysis-get
				   (caddr exp) env analysis)))
		  (if (and (not (abstract-all? car-answer))
			   (not (abstract-all? cdr-answer)))
		      (cons car-answer cdr-answer)
		      abstract-all)))
	       (else
		(refine-apply
		 (analysis-get (car exp) env analysis)
		 (analysis-get (cadr exp) env analysis)
		 analysis))))
	(else
	 (error "Invalid expression in abstract refiner"
		exp env analysis))))

(define (refine-analysis analysis)
  (map (lambda (binding)
	 (let ((exp (car binding))
	       (env (cadr binding)))
	   (list exp env (refine-eval exp env analysis))))
       (analysis-bindings analysis)))

;;; Given a particular abstract analysis, and a particular expression
;;; EXP and abstract environment ENV, the pair EXPAND-EVAL and
;;; EXPAND-APPLY will detect the set of once-removed
;;; expression-environment pairs that would shed light on the abstract
;;; evaluation of EXP in ENV but have not yet been analyzed at all
;;; (and return that set as a list of abstract bindings of those pairs
;;; to the abstract top value).

;;; In other words, these two expand the scope of which
;;; expression-environment pairs it appears are worth analyzing.

;;; EXPAND-APPLY is \bar A' from the paper.
(define (expand-apply proc arg analysis)
  (cond ((primitive? proc)
	 (if (or (abstract-all? arg) (not (eq? primitive-if proc)))
	     '()
	     (let ((predicate (car arg))
		   (consequent (cadr arg))
		   (alternate (cddr arg)))
	       (define (expand-thunk-application thunk)
		 (analysis-expand
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
	     (analysis-expand (closure-body proc)
			      (extend-env
			       (closure-formal proc)
			       arg
			       (closure-env proc))
			      analysis)))
	((abstract-all? proc)
	 '())
	(else
	 (error "Trying to expand on an application of something that is known not to be a procedure"
		proc arg analysis))))

;;; EXPAND-EVAL is \bar E' from the paper.
(define (expand-eval exp env analysis)
  (cond ((variable? exp) '())
	((null? exp) '())
	((constant? exp) '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		'())
	       ((eq? (car exp) 'cons)
		(lset-union
		 same-analysis-binding?
		 (analysis-expand (cadr exp) env analysis)
		 (analysis-expand (caddr exp) env analysis)))
	       (else
		(lset-union
		 same-analysis-binding?
		 (analysis-expand (car exp) env analysis)
		 (analysis-expand (cadr exp) env analysis)
		 (expand-apply
		  (analysis-get (car exp) env analysis)
		  (analysis-get (cadr exp) env analysis)
		  analysis)))))
	(else
	 (error "Invalid expression in abstract expander"
		exp env analysis))))

(define (analysis-expand-binding binding analysis)
  (let ((exp (car binding))
	(env (cadr binding)))
    (expand-eval exp env analysis)))

(define (expand-analysis analysis)
  (apply lset-union same-analysis-binding?
	 (map (lambda (binding)
		(analysis-expand-binding binding analysis))
	      (analysis-bindings analysis))))

(define (step-analysis analysis)
  (make-analysis
   (lset-union same-analysis-binding?
	       (refine-analysis analysis)
	       (expand-analysis analysis))))

(define *analyze-wallp* #f)

(define (analyze program)
  (let ((initial-analysis
	 (make-analysis
	  `((,(macroexpand program) ,(initial-vl-user-env) ,abstract-all)))))
    (let loop ((old-analysis initial-analysis)
	       (new-analysis (step-analysis initial-analysis))
	       (count 0))
      (if (and *analyze-wallp* (= 0 (modulo count *analyze-wallp*)))
	  (pp new-analysis))
      (if (step-changed-analysis? old-analysis new-analysis)
	  (loop new-analysis (step-analysis new-analysis) (+ count 1))
	  new-analysis))))
