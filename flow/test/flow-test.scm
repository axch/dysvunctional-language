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
   ))
