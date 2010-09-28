;;;; Polyvariant union-free flow analysis by abstract interpretation

;;; This file implements the resursive equations from page 7 of [1].
;;; The strategy is just to write those equations down directly as
;;; code and let it rip.  I'm sure a proper work-list algorithm would
;;; be much faster, but this program is supposed to be expository.

;;; The code extends and completes the presentation in [1] by filling
;;; out all the details.  In particular, [1] swept IF under the rug as
;;; a "primitive", but when I tried to write the program down it
;;; turned out to be somewhat thorny.  The thorns come from the fact
;;; that the analysis has to be kept from exploring any branch of an
;;; IF until that branch is known to be live.  This program does not
;;; include any additional subtleties that may be introduced by the AD
;;; basis.

;;; The main data structure being manipulated here is called an
;;; analysis.  (Sorry about the nomenclature -- "flow analysis" is the
;;; process, but an "analysis" is a particular collection of knowledge
;;; acquired by that process at a particular stage in its progress).
;;; Have a look at analysis.scm if you are interested in
;;; representation details.

;;; An analysis is a collection of bindings, binding pairs of
;;; expression and (abstract) environment to abstract values.  Every
;;; such binding means two things.  It means "I know that this
;;; expression, when evalutated in this abstract environment, will
;;; produce something of this shape (abstract value)."  It also means
;;; "I want to know more about what this expression will produce if
;;; evaluated in this abstract environment."

;;; The process of abstract interpretation with respect to a
;;; particular analysis consists of two parts: refinement and
;;; expansion.  Refinement tries to answer the question implicit in
;;; every binding by evaluating its expression in its environment for
;;; one step, referring to the analysis for the shapes of
;;; subexpressions and procedure results.  Refinement of one binding
;;; is the definitions of \bar E, \bar A, and \bar E_1 in [1], and
;;; REFINE-EVAL, REFINE-APPLY, and ANALYSIS-GET, respectively, here.

;;; Expansion asks more questions, by creating uninformative (but
;;; inquisitive) bindings for expression-environment pairs that it can
;;; tell would be useful for the refinement of some other, already
;;; present, binding.  Expansion also proceeds by evaluating the
;;; expression of every extant binding in the environment of that
;;; binding for one step, but instead of bottoming out in what the
;;; analysis knows, it only cares about whether the analysis is
;;; curious about those expressions that it finds.  Expansion is \bar
;;; E', \bar A', and \bar E'_1 in [1], and EXPAND-EVAL, EXPAND-APPLY,
;;; and ANALYSIS-EXPAND, respectively, here.

;;; The polyvariance of this flow analysis comes out of the fact that
;;; the same expression is allowed to appear paired with different
;;; abstract environments as the flow analysis proceeds, and the fates
;;; of such copies are henceforth separate.  The union-free-ness of
;;; this flow analysis comes out of the particular set of abstract
;;; values (allowable shapes) used; in particular from the fact that
;;; two shapes are considered compatible only if they differ just in
;;; which real numbers or which booleans occupy parallel slots and
;;; not, notably, if some closures they contain differ as to the
;;; closure's body.  See abstract-values.scm.

;;; [1] Jeffrey Siskind and Barak Pearlmutter, "Using Ployvariant
;;; Union-Free Flow Analysis to Compile a Higher-Order Functional
;;; Programming Language with a First-Class Derivative Operator to
;;; Efficient Fortran-like Code."  Purdue University ECE Technical
;;; Report, 2008.  http://docs.lib.purdue.edu/ecetr/376

;;;; Refinement

;;; REFINE-EVAL is \bar E from [1].
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

;;; REFINE-APPLY is \bar A from [1].
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
	 (error "Refining an application of a known non-procedure"
		proc arg analysis))))

(define (refine-analysis analysis)
  (map (lambda (binding)
	 (let ((exp (car binding))
	       (env (cadr binding)))
	   (list exp env (refine-eval exp env analysis))))
       (analysis-bindings analysis)))

;;;; Expansion

;;; EXPAND-EVAL is \bar E' from [1].
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

;;; EXPAND-APPLY is \bar A' from [1].
(define (expand-apply proc arg analysis)
  (cond ((primitive? proc)
	 (if (or (abstract-all? arg) (not (eq? primitive-if proc)))
	     '()
	     (let ((predicate (car arg))
		   (consequent (cadr arg))
		   (alternate (cddr arg)))
	       (define (expand-thunk-application thunk)
		 (analysis-expand
		  `(,(closure-expression thunk) ())
		  (closure-env thunk)
		  analysis))
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
	 (error "Expanding an application a known non-procedure"
		proc arg analysis))))

(define (analysis-expand-binding binding analysis)
  (let ((exp (car binding))
	(env (cadr binding)))
    (expand-eval exp env analysis)))

(define (expand-analysis analysis)
  (apply lset-union same-analysis-binding?
	 (map (lambda (binding)
		(analysis-expand-binding binding analysis))
	      (analysis-bindings analysis))))

;;;; Flow analysis

;;; STEP-ANALYSIS is U from [1].
(define (step-analysis analysis)
  (make-analysis
   (lset-union same-analysis-binding?
	       (refine-analysis analysis)
	       (expand-analysis analysis))))

(define *analyze-wallp* #f)

(define (analyze program)
  (let ((initial-analysis
	 (make-analysis
	  (list (list (macroexpand program)
		      (initial-vl-user-env)
		      abstract-all)))))
    (let loop ((old-analysis initial-analysis)
	       (new-analysis (step-analysis initial-analysis))
	       (count 0))
      (if (and *analyze-wallp* (= 0 (modulo count *analyze-wallp*)))
	  (pp new-analysis))
      (if (step-changed-analysis? old-analysis new-analysis)
	  (loop new-analysis (step-analysis new-analysis) (+ count 1))
	  new-analysis))))
