(declare (usual-integrations))

(define make-dvl-pair cons)
(define dvl-pair? pair?)
(define dvl-car car)
(define dvl-cdr cdr)
(define dvl-empty-list? null?)

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

(define (closure-free-variables closure)
  (free-variables (closure-exp closure)))

(define dvl-real? real?)

(define (object-map f object . objects)
  (cond ((closure? object)
	 (make-closure
	  (make-lambda-form
	   (closure-formal object)
	   (apply
	    expression-map f (closure-body object) (map closure-body objects)))
	  (apply f (closure-env object) (map closure-env objects))))
	((env? object)
	 (apply env-map f object objects))
	((dvl-pair? object)
	 (make-dvl-pair (apply f (dvl-car object) (map dvl-car objects))
			(apply f (dvl-cdr object) (map dvl-cdr objects))))
	(else
	 object)))

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

(define-structure
  (world
   safe-accessors
   (print-procedure
    (simple-unparser-method 'world
     (lambda (world)
       (list (world-gensym world))))))
  gensym)

(define (initial-dvl-world)
  (make-world 0))

(define impossible-world (make-world #f))
(define (impossible-world? thing)
  (eq? thing impossible-world))

(define (world-equal? world1 world2)
  (equal? (world-gensym world1) (world-gensym world2)))

(define-structure (gensym safe-accessors)
  number)

(define (union-world world1 world2)
  (cond ((impossible-world? world1) world2)
	((impossible-world? world2) world1)
	(else
	 (make-world
	  (max (world-gensym world1) (world-gensym world2))))))

(define (current-gensym world)
  (make-gensym (world-gensym world)))

(define (do-gensym world)
  (make-world
   (+ 1 (world-gensym world))))
