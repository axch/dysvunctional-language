(define (macroexpand exp)
  (cond ((variable? exp)
	 exp)
	((null? exp)
	 '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		`(lambda ,(macroexpand-formals (cadr exp))
		   ,(macroexpand (caddr exp))))
	       ((eq? (car exp) 'cons)
		`(cons ,(macroexpand (cadr exp))
		       ,(macroexpand (caddr exp))))
	       ((vl-macro? exp)
		(macroexpand (expand-vl-macro exp)))
	       (else
		`(,(macroexpand (car exp))
		  ,(macroexpand-operands (cdr exp))))))
	(else
	 (error "Invalid expression syntax" exp))))

(define (macroexpand-operands operands)
  (cond ((null? operands)
	 '())
	((null? (cdr operands))
	 (macroexpand (car operands)))
	(else
	 `(cons ,(macroexpand (car operands))
		,(macroexpand-operands (cdr operands))))))

(define (macroexpand-formals formals)
  (cond ((null? formals)
	 (list formals))
	((and (pair? formals) (null? (cdr formals)))
	 formals)
	((pair? formals)
	 (list
	  (let walk ((formals (apply cons* formals)))
	    (cond ((symbol? formals) formals)
		  ((null? formals) formals)
		  ((pair? formals)
		   (if (eq? (car formals) 'cons)
		       `(cons ,(walk (cadr formals))
			      ,(walk (caddr formals)))
		       `(cons ,(walk (car formals))
			      ,(walk (cdr formals)))))
		  (else
		   (error "Invalid formal parameter tree" formals))))))
	(else
	 (error "Invalid formal parameter tree" formals))))

(define *vl-macros* '())

(define (vl-macro? form)
  (memq (car form) (map car *vl-macros*)))

(define (expand-vl-macro form)
  (let ((transformer (assq (car form) *vl-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-vl-macro! name transformer)
  (set! *vl-macros* (cons (cons name transformer) *vl-macros*)))

(define-vl-macro! 'let
  (lambda (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      `((lambda ,(map car bindings)
	  ,@body)
	,@(map cadr bindings)))))

(define-vl-macro! 'if
  (lambda (form)
    `(if-procedure
      ,(cadr form)
      (lambda () ,(caddr form))
      (lambda () ,(cadddr form)))))
