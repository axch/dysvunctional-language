(declare (usual-integrations))
;;;; Expressions

(define (constant? thing)
  (or (number? thing)
      (boolean? thing)))

(define (variable? thing)
  (or (constant? thing)
      (symbol? thing)))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
	   (memq (car exp) '(cons vector real))
	   (every constructors-only? (cdr exp)))))

(define (count-in-tree thing tree)
  (cond ((equal? thing tree)
	 1)
	((pair? tree)
	 (+ (count-in-tree thing (car tree))
	    (count-in-tree thing (cdr tree))))
	(else 0)))

(define (occurs-in-tree? thing tree)
  (> (count-in-tree thing tree) 0))

(define (free-variables exp)
  (cond ((symbol? exp) (list exp))
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(lset-difference eq? (free-variables (caddr exp))
				 (free-variables (cadr exp))))
	       ((eq? (car exp) 'cons)
		(lset-union eq? (free-variables (cadr exp))
			    (free-variables (caddr exp))))
	       (else
		(lset-union eq? (free-variables (car exp))
			    (free-variables (cdr exp))))))
	(else '())))
;;;; Occurrences of free variables

(define (count-free-occurrences name exp)
  (cond ((eq? exp name) 1)
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(if (occurs-in-tree? name (cadr exp))
		    0
		    (count-free-occurrences name (cddr exp))))
	       ((eq? (car exp) 'let)
		(let ((bindings (cadr exp))
		      (body (cddr exp)))
		  (apply +
		   (if (memq name (map car bindings))
		       0
		       (count-free-occurrences name body))
		   (map (lambda (binding)
			  (count-free-occurrences name (cadr binding)))
			bindings))))
	       ((eq? (car exp) 'cons)
		(+ (count-free-occurrences name (cadr exp))
		   (count-free-occurrences name (caddr exp))))
	       (else
		(+ (count-free-occurrences name (car exp))
		   (count-free-occurrences name (cdr exp))))))
	(else 0)))

(define (replace-free-occurrences name new exp)
  (cond ((eq? exp name) new)
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(if (occurs-in-tree? name (cadr exp))
		    exp
		    `(lambda ,(cadr exp)
		       ,@(replace-free-occurrences name new (cddr exp)))))
	       ((eq? (car exp) 'let)
		(let ((bindings (cadr exp))
		      (body (cddr exp)))
		  `(let ,(map (lambda (binding)
				(list (car binding)
				      (replace-free-occurrences
				       name new (cadr binding))))
			      bindings)
		     ,@(if (memq name (map car bindings))
			   body
			   (replace-free-occurrences name new body)))))
	       ((eq? (car exp) 'cons)
		`(cons ,(replace-free-occurrences name new (cadr exp))
		       ,(replace-free-occurrences name new (caddr exp))))
	       (else
		(cons (replace-free-occurrences name new (car exp))
		      (replace-free-occurrences name new (cdr exp))))))
	(else exp)))
;;;; Closures

(define-structure (closure (safe-accessors #t) (constructor %make-closure))
  formal
  body
  free-variables
  env)

(define (closure-expression closure)
  `(lambda ,(closure-formal closure)
     ,(closure-body closure)))

(define (env-slice env symbols)
  (make-env
   (filter (lambda (binding)
	     (memq (car binding) symbols))
	   (env-bindings env))))

;;; To keep environments in canonical form, closures only keep the
;;; variables they want.
(define (make-closure formal body env)
  (let ((free (free-variables `(lambda ,formal ,body))))
    (%make-closure formal body free (env-slice env free))))

;;;; "Foreign Interface"
;;; such as it is

(define (vl-value->scheme-value thing)
  (cond ((pair? thing)
	 (cons (vl-value->scheme-value (car thing))
	       (vl-value->scheme-value (cdr thing))))
	((closure? thing)
	 (lambda args
	   (concrete-apply thing (map scheme-value->vl-value args))))
	(else
	 thing)))

(define (scheme-value->vl-value thing)
  thing)
