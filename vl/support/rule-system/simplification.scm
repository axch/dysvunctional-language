(declare (usual-integrations))

(define ((rule-list rules) data)
  (let per-rule ((rules rules))
    (if (null? rules)
	data
	(let ((answer ((car rules) data)))
	  (if (eq? data answer)
	      (per-rule (cdr rules))
	      answer)))))

(define (iterated the-rule)
  (lambda (data)
    (let loop ((data data)
	       (answer (the-rule data)))
      (if (eq? answer data)
	  answer
	  (loop answer (the-rule answer))))))

;; TODO on-subexpressions and rule-simplifier do not preserve eq?-ness
;; of their input even if they don't change it.  This is unfortunate.
(define (on-subexpressions the-rule)
  (define (on-expression expression)
    (let ((subexpressions-done
	   (if (list? expression)
	       (map on-expression expression)
	       expression)))
      (the-rule subexpressions-done)))
  on-expression)

(define (iterated-on-subexpressions the-rule)
  ;; Unfortunately, this is not just a composition of the prior two.
  (define (on-expression expression)
    (let ((subexpressions-done
	   (if (list? expression)
	       (map on-expression expression)
	       expression)))
      (let ((answer (the-rule subexpressions-done)))
	(if (eq? answer subexpressions-done)
	    answer
	    (on-expression answer)))))
  on-expression)

(define (rule-simplifier the-rules)
  (iterated-on-subexpressions (rule-list the-rules)))

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
