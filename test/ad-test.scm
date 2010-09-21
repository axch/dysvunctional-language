(define ad-eval
  (let ((ad-eval ad-eval))
    (lambda (form #!optional env)
      (if (default-object? env)
	  (set! env (make-ad-user-environment)))
      (ad-eval form env))))

(in-test-group
 ad

 (define-each-check
   (= 5 (ad-eval 5))
   (= 7 (ad-eval '(+ 2 5)))
   (equal? '(13 24) (ad-eval '(map + '(3 4) '(10 20))))
   (equal? '(25 36) (ad-eval '(map (lambda (x) (* x x)) '(5 6))))
   ))
