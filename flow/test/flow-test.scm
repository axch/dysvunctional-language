(define (analyzed-answer program)
  (let ((candidate
	 (assoc (macroexpand program)
		(analysis-bindings (analyze program)))))
    (if (not candidate)
	(error "Analysis makes no binding for the original program"
	       program)
	(caddr candidate))))

(in-test-group
 flow

 (define-each-check
   (equal? 3 (flow-eval 3))
   (equal? '((lambda (x) 1) (cons 3 ()))
	   (macroexpand '((lambda (x) 1) 3)))
   (equal? 1 (flow-eval '((lambda (x) 1) 3)))
   (equal? 3 (flow-eval '((lambda (x y) 3) 1 2)))
   (equal? '(lambda ((x . y)) x)
	   (macroexpand '(lambda ((cons x y)) x)))
   (equal? 3 (flow-eval '((lambda ((cons x y)) 3) (cons 1 2))))
   (equal? 2 (flow-eval '((lambda ((cons x y)) y) (cons 1 2))))
   (equal? '(1 . 3) (flow-eval '((lambda (x (y . z) w)
				   (cons x z))
				 1 (cons 2 3) 4)))
   (equal? 4 (flow-eval '(+ 1 3)))
   (equal? 3 (flow-eval '(let ((x 3)) x)))
   (equal? '(8 . 16)
	   (flow-eval '(let ((double (lambda (x)
				       (+ x x)))
			     (square (lambda (x)
				       (* x x)))
			     (compose (lambda (f g)
					(lambda (x) (f (g x))))))
			 (cons ((compose double square) 2)
			       ((compose square double) 2)))))

   (equal? 2 (analyzed-answer '((lambda (x) 2) 3)))
   (equal? 2 (analyzed-answer '((lambda (x y) 2) 3 4)))
   (equal? 3 (analyzed-answer '((lambda (x y) x) 3 4)))
   (equal? 4 (analyzed-answer '((lambda (x y) y) 3 4)))
   ))
