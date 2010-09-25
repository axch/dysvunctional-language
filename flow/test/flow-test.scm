(in-test-group
 flow

 (define-each-check
   (equal? 3 (flow-eval 3))
   (equal? '((lambda (x) 1) (cons 3 ()))
	   (macroexpand '((lambda (x) 1) 3)))
   (equal? 1 (flow-eval '((lambda (x) 1) 3)))))
