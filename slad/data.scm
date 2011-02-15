(declare (usual-integrations))

(define ((tagged-list? tag) thing)
  (and (pair? thing)
       (eq? (car thing) tag)))

(define (constant? thing)
  (or (number? thing)
      (boolean? thing)
      (slad-bundle? thing)
      (null? thing)
      (quoted? thing)))

(define (constant-value thing)
  (if (quoted? thing)
      (cadr thing)
      thing))

(define (variable? thing)
  (symbol? thing))
(define variable<? symbol<?)

(define definition? (tagged-list? 'define))

(define (normalize-definition definition)
  (cond ((not (definition? definition))
	 (error "Trying to normalize a non-definition" definition))
	((pair? (cadr definition))
	 (normalize-definition
	  `(define ,(caadr definition)
	     (lambda ,(cdadr definition)
	       ,@(cddr definition)))))
	(else
	 definition)))

(define (definiendum definition)
  (cadr (normalize-definition definition)))

(define (definiens definition)
  (caddr (normalize-definition definition)))

(define pair-form? (tagged-list? 'cons))
(define car-subform cadr)
(define cdr-subform caddr)
(define (make-pair-form car-subform cdr-subform)
  `(cons ,car-subform ,cdr-subform))

(define make-slad-pair cons)
(define slad-pair? pair?)
(define slad-car car)
(define slad-cdr cdr)
(define slad-empty-list? null?)

(define lambda-form? (tagged-list? 'lambda))
(define lambda-formal cadr)
(define lambda-body caddr)
(define (make-lambda-form formal body)
  `(lambda ,formal ,body))

(define (application? thing)
  (and (pair? thing)
       (not (pair-form? thing))
       (not (lambda-form? thing))))
(define operator-subform car)
(define operand-subform cadr)
(define (make-application operator-form operand-form)
  `(,operator-form ,operand-form))

(define quoted? (tagged-list? 'quote))

(define-structure (slad-closure safe-accessors (constructor %make-slad-closure))
  lambda
  env)

(define (slad-closure-formal closure)
  (lambda-formal (slad-closure-lambda closure)))

(define (slad-closure-body closure)
  (lambda-body (slad-closure-lambda closure)))

(define (env-slice env variables)
  (make-env
   (filter (lambda (binding)
	     (member (car binding) variables))
	   (env-bindings env))))

;;; To keep environments in canonical form, closures only keep the
;;; variables they want.
(define (make-slad-closure exp env)
  (let ((free (free-variables exp)))
    (%make-slad-closure exp (env-slice env free))))

(define-structure
  (slad-primitive
   safe-accessors
   (print-procedure
    (simple-unparser-method 'slad-primitive
     (lambda (prim)
       (list (slad-primitive-name prim))))))
  name
  implementation)

(define slad-real? real?)

(define-structure
  (slad-bundle
   safe-accessors
   (print-procedure
    (lambda (unparser-state bundle)
      (with-current-unparser-state unparser-state
	(lambda (port)
	  (with-output-to-port port
	    (lambda ()
	      (write (list 'forward (slad-bundle-primal bundle) (slad-bundle-tangent bundle))))))))))
  primal tangent)


(define (slad-map f object . objects)
  (cond ((slad-closure? object)
	 (make-slad-closure
	  (make-lambda-form
	   (slad-closure-formal object)
	   (apply slad-exp-map f (slad-closure-body object) (map slad-closure-body objects)))
	  (apply f (slad-closure-env object) (map slad-closure-env objects))))
	((env? object)
	 (apply slad-env-map f object objects))
	((slad-pair? object)
	 (make-slad-pair (apply f (slad-car object) (map slad-car objects))
			 (apply f (slad-cdr object) (map slad-cdr objects))))
	((slad-bundle? object)
	 (make-slad-bundle (apply f (slad-primal object) (map slad-primal objects))
			   (apply f (slad-tangent object) (map slad-tangent objects))))
	(else
	 object)))

(define (slad-exp-map f form . forms)
  (cond ((quoted? form)
	 `(quote ,(apply f (cadr form) (map cadr forms))))
	((constant? form)
	 (apply f form forms))
	((variable? form) form)
	((pair-form? form)
	 (make-pair-form (apply slad-exp-map f (car-subform form) (map car-subform forms))
			 (apply slad-exp-map f (cdr-subform form) (map cdr-subform forms))))
	((lambda-form? form)
	 (make-lambda-form (lambda-formal form)
			   (apply slad-exp-map f (lambda-body form) (map lambda-body forms))))
	((application? form)
	 (make-application (apply slad-exp-map f (operator-subform form) (map operator-subform forms))
			   (apply slad-exp-map f (operand-subform form) (map operand-subform forms))))
	(else
	 (error "Invalid expression type" form forms))))

(define (slad-copy object)
  (cond ((slad-primitive? object)
	 (make-slad-primitive (slad-primitive-name object)
			      (slad-primitive-implementation object)))
	(else
	 (slad-map slad-copy object))))

(define (memoize f)
  (let ((cache (make-eq-hash-table)))
    (lambda (x)
      (hash-table/lookup cache x
       (lambda (datum) datum)
       (lambda ()
	 (let ((answer (f x)))
	   (hash-table/put! cache x answer)
	   answer))))))

(define free-variables
  (memoize
   (lambda (form)
     (cond ((constant? form)
	    '())
	   ((variable? form)
	    (list form))
	   ((pair-form? form)
	    (lset-union equal? (free-variables (car-subform form))
			(free-variables (cdr-subform form))))
	   ((lambda-form? form)
	    (lset-difference equal? (free-variables (lambda-body form))
			     (free-variables (lambda-formal form))))
	   ((pair? form)
	    (lset-union equal? (free-variables (car form))
			(free-variables (cdr form))))
	   (else
	    (error "Invalid expression type" form forms))))))
