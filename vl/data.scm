(declare (usual-integrations))
;;;; Free variables

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

;;;; Closures

(define-structure (closure (safe-accessors #t) (constructor %make-closure))
  exp
  free-variables
  env)

(define (closure-formal closure)
  (lambda-formal (closure-exp closure)))

(define (closure-body closure)
  (lambda-body (closure-exp closure)))

(define (env-slice env variables)
  (make-env
   (filter (lambda (binding)
	     (member (car binding) variables))
	   (env-bindings env))))

;;; To keep environments in canonical form, closures only keep the
;;; variables they want.
(define (make-closure exp env)
  (let ((free (free-variables exp)))
    (%make-closure exp free (env-slice env free))))

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
