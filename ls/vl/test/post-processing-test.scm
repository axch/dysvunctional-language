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
       (operation-2 (real 5)))
    (compile-to-scheme
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
    (compile-to-scheme
     '(let ((double (lambda (x) (+ x x)))
            (square (lambda (x) (* x x)))
            (compose (lambda (f g) (lambda (x) (f (g x))))))
        (cons ((compose double square) (real 2))
              ((compose square double) (real 2))))))

   ;; Note the extra variable the-closure-53, below.  This is a use
   ;; case for interprocedural alias elimination.
   (alpha-rename?
    '(begin
       (define (operation-9 the-closure-41
                            the-closure-53
                            the-formals-56
                            the-formals-57)
         (argument-types real real real real real)
         (if (< (abs (- the-formals-56 the-formals-57)) .00001)
             the-formals-57
             (operation-9
              the-closure-53
              the-closure-53
              the-formals-57
              (/ (+ the-formals-57 (/ the-closure-41 the-formals-57)) 2))))
       (let ((the-formals-125 (real 2)))
         (let ((the-formals-133 (real 1.)))
           (cons
            1.4142135623730951
            (operation-9
             the-formals-125
             the-formals-125
             the-formals-133
             (/ (+ the-formals-133 (/ the-formals-125 the-formals-133)) 2))))))
    (compile-to-scheme
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
