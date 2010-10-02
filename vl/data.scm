(declare (usual-integrations))
;;;; Expressions

(define (constant? thing)
  (or (number? thing)
      (boolean? thing)))

(define (variable? thing)
  (or (constant? thing)
      (symbol? thing)))

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

(define (replace-in-tree old new structure)
  (cond ((eq? structure old) new)
	((pair? structure)
	 (cons (replace-in-tree old new (car structure))
	       (replace-in-tree old new (cdr structure))))
	(else structure)))

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
