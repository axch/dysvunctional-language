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

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
	   (memq (car exp) '(cons vector real))
	   (every constructors-only? (cdr exp)))))

(define (count-in-tree thing tree)
  (cond ((equal? thing tree) 1)
	((pair? tree)
	 (+ (count-in-tree thing (car tree))
	    (count-in-tree thing (cdr tree))))
	(else 0)))

(define (occurs-in-tree? thing tree)
  (> (count-in-tree thing tree) 0))

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
