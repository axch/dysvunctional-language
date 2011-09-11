(declare (usual-integrations))
(in-test-group
 post-processing

 (define-each-check
   

   (alpha-rename?
    '(begin
       (define (operation-2 n)
         (argument-types real real)
         (if (= n 1)
             1
             (* n (operation-2 (- n 1)))))
       (let ((n-25 (real 5)))
         (if (= n-25 1)
             1
             (* n-25 (operation-2 (- n-25 1))))))
    (compile-to-fol
     '(let ()
        (define (fact n)
          (if (= n 1)
              1
              (* n (fact (- n 1)))))
        (fact (real 5)))))

   (alpha-rename?
    '(let ((z1 (real 2)))
       (let ((x1 (* z1 z1)))
         (let ((z2 (real 2)))
           (let ((x2 (+ z2 z2)))
             (cons (+ x1 x1)
                   (* x2 x2))))))
    (compile-to-fol
     '(let ((double (lambda (x) (+ x x)))
            (square (lambda (x) (* x x)))
            (compose (lambda (f g) (lambda (x) (f (g x))))))
        (cons ((compose double square) (real 2))
              ((compose square double) (real 2))))))

   (alpha-rename?
    '(begin
       (define (operation-2 the-closure the-formals-215 the-formals-216)
         (argument-types real real real real)
         (if (< (abs (- the-formals-215 the-formals-216)) .00001)
             the-formals-216
             (operation-2 the-closure
                          the-formals-216
                          (/ (+ the-formals-216 (/ the-closure the-formals-216))
                             2))))
       (let ((the-formals-64 (real 2)))
         (let ((anf-170 (real 1.)))
           (let ((anf-152 (/ (+ anf-170 (/ the-formals-64 anf-170)) 2)))
             (cons
              1.4142135623730951
              (if (< (abs (- anf-170 anf-152)) .00001)
                  anf-152
                  (operation-2 the-formals-64
                               anf-152
                               (/ (+ anf-152 (/ the-formals-64 anf-152)) 2))))))))
    (compile-to-fol
     '(let ()
        (define (heron-step x)
          (lambda (guess)
            (/ (+ guess (/ x guess)) 2)))
        (define (close-enuf? a b)
          (< (abs (- a b)) .00001))
        (define (numeric-fix f start close-enuf?)
          (let loop ((old start) (new (f start)))
            (if (close-enuf? old new)
                new
                (loop new (f new)))))
        (define (square-root x)
          (numeric-fix (heron-step x) (real 1.) close-enuf?))
        (cons (sqrt 2) (square-root (real 2))))))
   ))
