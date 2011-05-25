(define *ad-macros* '())

(define (ad-macro? form)
  (memq (car form) (map car *ad-macros*)))

(define (ad-transform form)
  (let ((transformer (assq (car form) *ad-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Unknown macro" form))))

(define (define-ad-macro name transformer)
  (set! *ad-macros* (cons (cons name transformer)
			  *ad-macros*)))

(define (normal-let-transform form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    `((lambda ,(map car bindings)
	,@body)
      ,@(map cadr bindings))))

(define (loopy-let-transform form)
  (let ((name (cadr form))
	(bindings (caddr form))
	(body (cdddr form)))
    `(letrec ((,name (lambda ,(map car bindings)
		       ,@body)))
       (,name ,@(map cadr bindings)))))

(define-ad-macro 'let
  (lambda (form)
    (if (symbol? (cadr form))
	(loopy-let-transform form)
	(normal-let-transform form))))

(define-ad-macro 'let*
  (lambda (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      (if (not (null? bindings))
	  `(let (,(car bindings))
	     (let* ,(cdr bindings)
	       ,@body))
	  `(let ()
	     ,@body)))))
