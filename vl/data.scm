(declare (usual-integrations))
;;;; Free variables

(define (free-variables exp)
  (cond ((variable? exp) (list exp))
	((lambda-form? exp)
	 (lset-difference equal? (free-variables (lambda-body exp))
			  (free-variables (lambda-formal exp))))
	((pair-form? exp)
	 (lset-union equal? (free-variables (car-subform exp))
		     (free-variables (cdr-subform exp))))
	((pair? exp) ; Not application? to handle formals lists
	 (lset-union equal? (free-variables (car exp))
		     (free-variables (cdr exp))))
	(else '())))

;;;; Closures

(define-structure
  (closure
   safe-accessors
   (constructor %make-closure)
   (print-procedure
    (simple-unparser-method 'closure
     (lambda (closure)
       (list (closure-exp closure)
	     (closure-env closure))))))
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
