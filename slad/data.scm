(declare (usual-integrations))

(define make-slad-pair cons)
(define slad-pair? pair?)
(define slad-car car)
(define slad-cdr cdr)
(define slad-empty-list? null?)

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
    (%make-closure exp (env-slice env free))))

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

(define (object-map f object)
  (cond ((closure? object)
	 (make-closure
	  (expression-map f (closure-exp object))
	  (f (closure-env object))))
	((env? object)
	 (env-map f object))
	((slad-pair? object)
	 (make-slad-pair (f (slad-car object)) (f (slad-cdr object))))
	((slad-bundle? object)
	 (make-slad-bundle (f (slad-primal object)) (f (slad-tangent object))))
	(else
	 object)))

(define (congruent-map f object1 object2 lose)
  (cond ((and (closure? object1) (closure? object2))
	 (make-closure
	  ;; TODO lose (not error) if expressions not congruent
	  (expression-map f (closure-exp object1) (closure-exp object2))
	  (f (closure-env object1) (closure-env object2))))
	((and (env? object1) (env? object2))
	 (env-map f object1 object2))
	((and (slad-pair? object1) (slad-pair? object2))
	 (make-slad-pair (f (slad-car object1) (slad-car object2))
			 (f (slad-cdr object1) (slad-cdr object2))))
	((and (slad-bundle? object1) (slad-bundle? object2))
	 (make-slad-bundle
	  (f (slad-primal object1) (slad-primal object2))
	  (f (slad-tangent object1) (slad-tangent object2))))
	(else
	 (lose))))

(define (expression-map f form . forms)
  (cond ((quoted? form)
	 `(quote ,(apply f (cadr form) (map cadr forms))))
	((constant? form)
	 (apply f form forms))
	((variable? form) form)
	((pair-form? form)
	 (make-pair-form
	  (apply expression-map f (car-subform form) (map car-subform forms))
	  (apply expression-map f (cdr-subform form) (map cdr-subform forms))))
	((lambda-form? form)
	 (make-lambda-form
	  (lambda-formal form)
	  (apply expression-map f (lambda-body form) (map lambda-body forms))))
	((application? form)
	 (make-application
	  (apply expression-map f (operator-subform form) (map operator-subform forms))
	  (apply expression-map f (operand-subform form) (map operand-subform forms))))
	(else
	 (error "Invalid expression type" form forms))))

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
