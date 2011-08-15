(declare (usual-integrations))
(in-test-group
 cse

 (define-each-check
   ;; Eliminate common (+ x 2)
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ x 2)))
         (+ y y)))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 2)))
          (let ((z (+ x 2)))
            (+ y z))))))

   ;; Eliminate common (+ x 2) in parallel let
   (equal?
    '(let ((x (real 5)))
       ;; I could improve this by hacking intraprocedural-cse more,
       ;; or I can just let dead code elimination deal with it.
       (let ((y (+ x 2)) (z (+ x 2)))
         (+ y y)))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 2)) (z (+ x 2)))
          (+ y z)))))

   ;; See through (+ x 0)
   (equal?
    '(let ((x (real 5)))
       (+ x x))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 0)))
          (let ((z (+ x 0)))
            (+ y z))))))

   ;; TODO Abstract the test pattern "CSE didn't eliminate this thing
   ;; it shouldn't have eliminated"?

   ;; Leave non-common things be
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ x 3)))
         y))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 3)))
          y))))

   ;; Respect variable scope
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         w))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          w))))

   ;; Continue respecting variable scope
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         (let ((z (+ x 3)))
           (+ w z))))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          (let ((z (+ x 3)))
            (+ w z))))))

   ;; But don't let variable scope interfere with finding other common
   ;; expressions
   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         (let ((z (+ x 3)))
           (+ w (* z z)))))
    (%intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          (let ((z (+ x 3)))
            (let ((u (+ x 3)))
              (+ w (* z u))))))))

   ;; This CSE is intraprocedural
   (equal?
    '(begin
       (define (op a b)
         (argument-types real real (values real real))
         (values (+ a b) (- a b)))
       (let-values (((x y) (op 1 2)))
         (+ x y)))
    (intraprocedural-cse
     '(begin
        (define (op a b)
          (argument-types real real (values real real))
          (values (+ a b) (- a b)))
        (let-values (((x y) (op 1 2)))
          (+ x y)))))

   ;; I know that x+1 is (+ x 1) in both branches of the IF
   (equal?
    '(let ((x (real 3)))
       (let-values
           (((x+1 something)
             (if (> (real 2) 1)
                 (values (+ x 1) (+ x 4))
                 (values (+ x 1) (+ x 3)))))
         (* something (+ x+1 x+1))))
    (intraprocedural-cse
     '(let ((x (real 3)))
        (let-values (((x+1 something)
                      (if (> (real 2) 1)
                          (values (+ x 1) (+ x 4))
                          (values (+ x 1) (+ x 3)))))
          (let ((y (+ x 1)))
            (* something (+ y x+1)))))))

   ;; Do not collapse procedures that have side effects
   (equal?
    '(let ((x (read-real))
           (y (read-real)))
       (+ x y))
    (intraprocedural-cse
     '(let ((x (read-real))
            (y (read-real)))
        (+ x y))))

   ;; But do collapse their outputs, once bound
   (equal?
    '(let ((x (read-real)))
       (+ x x))
    (intraprocedural-cse
     '(let ((x (read-real)))
        (let ((y x))
          (+ x y)))))

   ;; Do not collapse user procedures that have side effects
   (equal?
    '(begin
       (define (my-read)
         (read-real))
       (let ((x (my-read))
             (y (my-read)))
         (+ x y)))
    (intraprocedural-cse
     '(begin
        (define (my-read)
          (read-real))
        (let ((x (my-read))
              (y (my-read)))
          (+ x y)))))

   ;; Do not collapse calls to REAL -- we're not supposed to know what
   ;; the constant inside is.
   (equal?
    '(let ((x (real 1)))
       (let ((y (real 1)))
         (+ x y)))
    (intraprocedural-cse
     '(let ((x (real 1)))
        (let ((y (real 1)))
          (+ x y)))))

   ;; Algebraic simplification can expose CSE opportunities,
   ;; even from nice simplifications like (* foo 1) -> foo
   (equal?
    '(let ((foo (real 2)))
       (let ((bar (+ foo 1)))
         (+ bar bar)))
    (intraprocedural-cse
     '(let ((foo (real 2)))
        (let ((bar (+ foo 1)))
          (let ((x (* foo 1)))
            (+ bar (+ x 1)))))))

   ;; Lambda expressions are not the same as their bodies
   (equal?
    '(let ((y (real 3)))
       (let ((z (lambda (formal)
                  y)))
         z))
    (intraprocedural-cse
     '(let ((y (real 3)))
        (let ((z (lambda (formal)
                   y)))
          z))))

   ;; Treating CONS as a pure procedure collapses EQUAL? pairs into
   ;; structure sharing.
   (equal?
    '(let ((x (cons 1 2)))
       (cons x x))
    (intraprocedural-cse
     '(let ((x (cons 1 2)))
        (let ((y (cons 1 2)))
          (cons x y)))))

   ;; With appropriate simplifications, CSE works through locally
   ;; constructed structures.
   (equal?
    '(let ((x (cons 1 2)))
       1)
    (intraprocedural-cse
     '(let ((x (cons 1 2)))
        (let ((y (car x)))
          y))))

   (equal?
    '(let ((z (real 1)))
       (let ((x (cons z 2)))
         z))
    (intraprocedural-cse
     '(let ((z (real 1)))
        (let ((x (cons z 2)))
          (let ((y (car x)))
            y)))))

   ;; TODO An example for interprocedural CSE
   #;
   (begin
     (define (double-fact n1 n2)
       (argument-types real real (values real real))
       (if (<= n1 1)
           (values n1 n2)
           (let-values (((n1-1! n2-1!) (double-fact (- n1 1) (- n2 1))))
             (values (* n1 n1-1!) (* n2 n2-1!)))))
     (let ((x (real 5)))
       (let-values (((a1 a2) (double-fact x x)))
         (cons a1 a2))))

   ))
