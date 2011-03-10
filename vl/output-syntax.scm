(declare (usual-integrations))
;;;; Syntax and manipulations of the output language

(define (try-rule-toplevel the-rule)
  (lambda (data)
    (the-rule data (lambda (value fail) value) (lambda () data))))

(define let-form? (tagged-list? 'let))

(define ->lambda
  (try-rule-toplevel
   (rule `(let (? bindings) (?? body))
	 `((lambda ,(map car bindings)
	     ,@body)
	   ,@(map cadr bindings)))))

(define ->let
  (try-rule-toplevel
   (rule `((lambda (? names) (?? body)) (?? args))
	 `(let ,(map list names args) ,@body))))

(define reconstitute-definition
  (try-rule-toplevel
   (rule `(define (? name)
	    (lambda (? names)
	      (?? body)))
	 `(define (,name ,@names)
	    ,@body))))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
	   (memq (car exp) '(cons vector real car cdr vector-ref))
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
	((lambda-form? exp)
	 (if (occurs-in-tree? name (lambda-formal exp))
	     0
	     (count-free-occurrences name (cddr exp))))
	((let-form? exp)
	 (let ((bindings (cadr exp))
	       (body (cddr exp)))
	   (apply +
		  (if (memq name (map car bindings))
		      0
		      (count-free-occurrences name body))
		  (map (lambda (binding)
			 (count-free-occurrences name (cadr binding)))
		       bindings))))
	((pair-form? exp)
	 (+ (count-free-occurrences name (car-subform exp))
	    (count-free-occurrences name (cdr-subform exp))))
	((pair? exp)
	 (+ (count-free-occurrences name (car exp))
	    (count-free-occurrences name (cdr exp))))
	(else 0)))

(define (replace-free-occurrences name new exp)
  (cond ((eq? exp name) new)
	((lambda-form? exp)
	 (if (occurs-in-tree? name (lambda-formal exp))
	     exp
	     `(lambda ,(lambda-formal exp)
		,@(replace-free-occurrences name new (cddr exp)))))
	((let-form? exp)
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
	((pair-form? exp)
	 `(cons ,(replace-free-occurrences name new (car-subform exp))
		,(replace-free-occurrences name new (cdr-subform exp))))
	((pair? exp)
	 (cons (replace-free-occurrences name new (car exp))
	       (replace-free-occurrences name new (cdr exp))))
	(else exp)))

(define (alpha-extend env names)
  (append
   (map cons
	names
	(map (lambda (name)
	       (if (assq name env)
		   (make-name (symbol name '-))
		   name))
	     names))
   env))

(define (alpha-rename exp env)
  (cond ((assq exp env) => cdr)
	((lambda-form? exp)
	 (let ((names (lambda-formal exp))
	       (body (lambda-body exp)))
	   (let ((new-env (alpha-extend env names)))
	     (make-lambda-form
	      (map (lambda (name)
		     (cdr (assq name new-env)))
		   names)
	      (alpha-rename body new-env)))))
	((let-form? exp)
	 (->let (alpha-rename (->lambda exp) env)))
	((definition? exp)
	 ;; Assume the definiendum is already unique
	 (reconstitute-definition
	  `(define ,(definiendum exp)
	     ,(alpha-rename (definiens exp) env))))
	((pair? exp)
	 (cons (alpha-rename (car exp) env) (alpha-rename (cdr exp) env)))
	(else exp)))
