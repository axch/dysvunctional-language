(define ad-eval
  (let ((ad-eval ad-eval))
    (lambda (form #!optional env)
      (if (default-object? env)
	  (set! env (make-ad-user-environment)))
      (ad-eval form env))))

(define (fad-eval form)
  (perturbed-eval form (make-ad-user-environment) the-non-perturbation))

(in-test-group
 ad

 (define-each-check
   (= 5 (ad-eval 5))
   (= 7 (ad-eval '(+ 2 5)))
   (equal? '(13 24) (ad-eval '(map + '(3 4) '(10 20))))
   (equal? '(25 36) (ad-eval '(map (lambda (x) (* x x)) '(5 6)))))

 (define-each-check
   (= 5 (fad-eval 5))
   (= 7 (fad-eval '(+ 2 5)))
   (equal? '(13 24) (fad-eval '(map + '(3 4) '(10 20))))
   (equal? '(25 36) (fad-eval '(map (lambda (x) (* x x)) '(5 6))))
   (equal? '(0 . 1) (fad-eval '((j* sin) 0 1)))
   (equal? '(0 . 1) (fad-eval '((j* (lambda (x) (sin x))) 0 1)))
   (equal? '(36 . 12) (fad-eval '((j* (lambda (x) (* x x))) 6 1))))

)

#;
(lambda (f)
  (lambda (primal)
    (cdr ((j* f) primal 1))))
