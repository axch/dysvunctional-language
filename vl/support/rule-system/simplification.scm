(declare (usual-integrations))

(define (rule-simplifier the-rules)
  (define (simplify-expression expression)
    (let ((subexpressions-simplified
	   (if (list? expression)
	       (map simplify-expression expression)
	       expression)))
      (try-rules subexpressions-simplified the-rules
       (lambda (result fail)
	 (simplify-expression result))
       (lambda ()
	 subexpressions-simplified))))
  (rule-memoize simplify-expression))

(define (recursively-try-once the-rule)
  (define (simplify-expression expression)
    (let ((subexpressions-simplified
	   (if (list? expression)
	       (map simplify-expression expression)
	       expression)))
      (try-rules subexpressions-simplified (list the-rule)
       (lambda (result fail) result)
       (lambda () subexpressions-simplified))))
  (rule-memoize simplify-expression))

(define (list<? x y)
  (let ((nx (length x)) (ny (length y)))
    (cond ((< nx ny) #t)
	  ((> nx ny) #f)
	  (else
	   (let lp ((x x) (y y))
	     (cond ((null? x) #f)	; same
		   ((expr<? (car x) (car y)) #t)
		   ((expr<? (car y) (car x)) #f)
		   (else (lp (cdr x) (cdr y)))))))))

(define expr<?
  (make-entity
   (lambda (self x y)
     (let per-type ((types (entity-extra self)))
       (if (null? types)
	   (error "Unknown expression type -- expr<?" x y)
	   (let ((predicate? (caar types))
		 (comparator (cdar types)))
	     (cond ((predicate? x)
		    (if (predicate? y)
			(comparator x y)
			#t))
		   ((predicate? y) #f)
		   (else (per-type (cdr types))))))))
   `((,null?    . ,(lambda (x y) #f))
     (,boolean? . ,(lambda (x y) (and (eq? x #t) (eq? y #f))))
     (,number?  . ,<)
     (,symbol?  . ,symbol<?)
     (,list?    . ,list<?))))
