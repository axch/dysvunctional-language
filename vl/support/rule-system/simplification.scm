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

(define (try-subexpressions the-rule expression)
  (if (list? expression)
      (let ((subexpressions-tried (map the-rule expression)))
        (if (every eq? expression subexpressions-tried)
            expression
            subexpressions-tried))
      expression))

(define (on-subexpressions the-rule)
  (define (on-expression expression)
    (let ((subexpressions-done (try-subexpressions on-expression expression)))
      (the-rule subexpressions-done)))
  on-expression)

(define (iterated-on-subexpressions the-rule)
  ;; Unfortunately, this is not just a composition of the prior two.
  (define (on-expression expression)
    (let ((subexpressions-done (try-subexpressions on-expression expression)))
      (let ((answer (the-rule subexpressions-done)))
	(if (eq? answer subexpressions-done)
	    answer
	    (on-expression answer)))))
  on-expression)

(define (rule-simplifier the-rules)
  (iterated-on-subexpressions (rule-list the-rules)))

(define (top-down the-rule)
  (define (on-expression expression)
    (let ((answer (the-rule expression)))
      (if (eq? answer expression)
          (let ((subexpressions-done
                 (try-subexpressions on-expression expression)))
            (let ((answer (the-rule subexpressions-done)))
              (if (eq? answer subexpressions-done)
                  answer
                  (on-expression answer))))
          (on-expression answer))))
  on-expression)

(define (in-order . the-rules)
  (lambda (datum)
    (let loop ((the-rules the-rules)
               (datum datum))
      (if (null? the-rules)
          datum
          (loop (cdr the-rules)
                ((car the-rules) datum))))))

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
