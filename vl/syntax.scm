(declare (usual-integrations))
;;;; Expressions

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

(define quoted? (tagged-list? 'quote))

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
