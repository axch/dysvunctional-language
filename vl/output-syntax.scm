(declare (usual-integrations))
;;;; Syntax and manipulations of the output language

(define let-form? (tagged-list? 'let))

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
