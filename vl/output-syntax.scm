(declare (usual-integrations))
;;;; Syntax and manipulations of the output language

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
