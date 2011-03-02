(declare (usual-integrations))

(define ((tagged-list? tag) thing)
  (and (pair? thing)
       (eq? (car thing) tag)))

(define (constant? thing)
  (or (number? thing)
      (boolean? thing)
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
	(else definition)))

(define (definiendum definition)
  (cadr (normalize-definition definition)))

(define (definiens definition)
  (caddr (normalize-definition definition)))

(define pair-form? (tagged-list? 'cons))
(define car-subform cadr)
(define cdr-subform caddr)
(define (make-pair-form car-subform cdr-subform)
  `(cons ,car-subform ,cdr-subform))

(define make-dvl-pair cons)
(define dvl-pair? pair?)
(define dvl-car car)
(define dvl-cdr cdr)
(define dvl-empty-list? null?)

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

(define-structure (closure safe-accessors (constructor %make-closure))
  lambda
  env)

(define (closure-formal closure)
  (lambda-formal (closure-lambda closure)))

(define (closure-body closure)
  (lambda-body (closure-lambda closure)))

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

(define dvl-real? real?)

(define (dvl-map f object . objects)
  (cond ((closure? object)
	 (make-closure
	  (make-lambda-form
	   (closure-formal object)
	   (apply
	    dvl-exp-map f (closure-body object) (map closure-body objects)))
	  (apply f (closure-env object) (map closure-env objects))))
	((env? object)
	 (apply dvl-env-map f object objects))
	((dvl-pair? object)
	 (make-dvl-pair (apply f (dvl-car object) (map dvl-car objects))
			(apply f (dvl-cdr object) (map dvl-cdr objects))))
	(else
	 object)))

(define (dvl-exp-map f form . forms)
  (cond ((quoted? form)
	 `(quote ,(apply f (cadr form) (map cadr forms))))
	((constant? form)
	 (apply f form forms))
	((variable? form) form)
	((pair-form? form)
	 (make-pair-form
	  (apply dvl-exp-map f (car-subform form) (map car-subform forms))
	  (apply dvl-exp-map f (cdr-subform form) (map cdr-subform forms))))
	((lambda-form? form)
	 (make-lambda-form
	  (lambda-formal form)
	  (apply dvl-exp-map f (lambda-body form) (map lambda-body forms))))
	((application? form)
	 (make-application
	  (apply dvl-exp-map f (operator-subform form) (map operator-subform forms))
	  (apply dvl-exp-map f (operand-subform form) (map operand-subform forms))))
	(else
	 (error "Invalid expression type" form forms))))

(define (dvl-copy object)
  (cond ((dvl-primitive? object)
	 (make-dvl-primitive (dvl-primitive-name object)
			     (dvl-primitive-implementation object)))
	(else
	 (dvl-map dvl-copy object))))

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

(define-structure (world (safe-accessors #t))
  io-version
  gensym)

(define (initial-dvl-world)
  (make-world 0 0))

(define impossible-world (make-world #f #f))
(define (impossible-world? thing)
  (eq? thing impossible-world))

(define any-world (make-world #t #t))
(define (any-world? thing)
  (eq? thing any-world))

(define (world-matches? world pattern)
  (or (any-world? pattern)
      (and (not (impossible-world? pattern))
	   (not (impossible-world? world))
	   (= (world-gensym world)
	      (world-gensym pattern)))))

(define (world-equal? world1 world2)
  (and (equal? (world-io-version world1) (world-io-version world2))
       (equal? (world-gensym world1) (world-gensym world2))))

(define-structure (gensym safe-accessors)
  number)

(define (do-i/o world)
  (make-world
   (+ 1 (world-io-version world))
   (world-gensym world)))

(define (union-world world1 world2)
  (make-world
   (max (world-io-version world1) (world-io-version world2))
   (max (world-gensym world1) (world-gensym world2))))

(define (current-gensym world)
  (make-gensym (world-gensym world)))

(define (do-i/o world)
  (make-world
   (world-io-version world)
   (+ 1 (world-gensym world))))
