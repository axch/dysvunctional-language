(define (fad-eval form)
  (set! *epsilon-count* 0)
  (perturbed-eval form (make-ad-user-environment) the-non-perturbation))

(in-test-group
 ad

 (define-each-check
   (= 5 (fad-eval 5))
   (= 7 (fad-eval '(+ 2 5)))
   (= 5 (fad-eval '(let ((x 5)) x)))
   (= 1/4 (fad-eval '(let ((x (+ 5 1)) (y (* 3 8))) (/ x y))))
   (= 54 (fad-eval '(let* ((x (+ 5 1)) (y (* x 8))) (+ x y))))
   (equal? '(13 24) (fad-eval '(map + '(3 4) '(10 20))))
   (equal? '(25 36) (fad-eval '(map (lambda (x) (* x x)) '(5 6))))
   (equal? '(0 . 1) (fad-eval '((j* sin) 0 1)))
   (equal? '(0 . 1) (fad-eval '((j* (lambda (x) (sin x))) 0 1)))
   (equal? '(36 . 12) (fad-eval '((j* (lambda (x) (* x x))) 6 1)))
   (equal? 12 (fad-eval '((lambda (y) (cdr ((j* (lambda (x) (* x (* x x)))) y 1))) 2)))
   (equal? '(12 . 12) (fad-eval '((j* (lambda (y) (cdr ((j* (lambda (x) (* x (* x x)))) y 1)))) 2 1)))
   )

)

#;
(lambda (f)
  (lambda (primal)
    (cdr ((j* f) primal 1))))
