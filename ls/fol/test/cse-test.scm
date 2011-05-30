(in-test-group
 cse
 
 (define-each-check
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ x 2)))
         (+ y y)))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 2)))
          (let ((z (+ x 2)))
            (+ y z))))))

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
   (equal?
    '(let ((x (real 5)))
       (let ((y (+ x 3)))
         y))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((y (+ x 3)))
          y))))

   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         w))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          w))))

   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         (let ((z (+ x 3)))
           (+ w z))))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          (let ((z (+ x 3)))
            (+ w z))))))

   (equal?
    '(let ((x (real 5)))
       (let ((w (let ((y (+ x 3))) y)))
         (let ((z (+ x 3)))
           (+ w (* z z)))))
    (intraprocedural-cse
     '(let ((x (real 5)))
        (let ((w (let ((y (+ x 3))) y)))
          (let ((z (+ x 3)))
            (let ((u (+ x 3)))
              (+ w (* z u))))))))

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

   (equal?
    '(let ((x (read-real))
           (y (read-real)))
       (+ x y))
    (intraprocedural-cse
     '(let ((x (read-real))
            (y (read-real)))
        (+ x y))))

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
