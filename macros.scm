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

(define-ad-macro 'let
  (lambda (form)
    (let ((bindings (cadr form))
	  (body (cddr form)))
      `((lambda ,(map car bindings) ,@body)
	,@(map cadr bindings)))))
