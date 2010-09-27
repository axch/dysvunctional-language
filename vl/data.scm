(define (constant? thing)
  (or (number? thing)
      (boolean? thing)))

(define (variable? thing)
  (or (constant? thing)
      (symbol? thing)))

(define-structure (closure (safe-accessors #t))
  formal
  body
  env)

(define (closure-expression closure)
  `(lambda ,(closure-formal closure)
     ,(closure-body closure)))

(define (vl-value->scheme-value thing)
  (cond ((pair? thing)
	 (cons (vl-value->scheme-value (car thing))
	       (vl-value->scheme-value (cdr thing))))
	((closure? thing)
	 (lambda args
	   (concrete-apply thing (map scheme-value->vl-value args))))
	(else
	 thing)))

(define (scheme-value->vl-value thing)
  thing)

(define (replace-in-tree old new structure)
  (cond ((eq? structure old) new)
	((pair? structure)
	 (cons (replace-in-tree old new (car structure))
	       (replace-in-tree old new (cdr structure))))
	(else structure)))
