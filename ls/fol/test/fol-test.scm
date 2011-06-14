(declare (usual-integrations))
(in-test-group
 fol

 (define-each-check
   (equal?
    '(begin
       (vector-ref (vector 1 2) 0))
    (structure-definitions->vectors
     '(begin
        (define-typed-structure foo (bar real) (baz real))
        (foo-bar (make-foo 1 2)))))

   (lset= eq? '(c a) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))

   (lset= eq? '(a c d)
          (feedback-vertex-set
           ;; This is G from the comments in feedback-vertex-set.scm
           '((a b) (b a c) (c a e) (d c e) (e d))))

   ;; Some fooling around with SRA and conditionals
   (alpha-rename?
    '(if (< (real 1) (real 2))
         (real 0)
         (+ (real 1) (real 2)))
    (fol-optimize
     '(car
       (let ((x (let ((y (+ (real 1) (real 2))))
                  (cons y (vector)))))
         (if (< (real 1) (real 2))
             (cons (real 0) (vector))
             x)))))

   (alpha-rename?
    '(if (< (real 1) (real 2))
         (real 0)
         (+ (real 1) (real 2)))
    (fol-optimize
     '(car
       (let ((x (let ((y (+ (real 1) (real 2))))
                  (cons y (cons y (vector))))))
         (if (< (real 1) (real 2))
             (cons (real 0) (cons (real 1) (vector)))
             x)))))

   (alpha-rename?
    '(+ (real 1) (real 2))
    (fol-optimize
     '(car
       (cdr
        (let ((x (let ((y (+ (real 1) (real 2))))
                   (cons y (cons y (vector))))))
          (if (< (real 1) (real 2))
              (cons (real 0) x)
              (cons (real 1) x)))))))

   (alpha-rename?
    '(let ((y (+ (real 1) (real 2))))
       (if (< (real 1) (real 2))
           (+ y (real 0))
           y))
    (fol-optimize
     '(let ((x (let ((y (+ (real 1) (real 2))))
                 (cons (+ y (real 0)) (cons y (vector))))))
        (if (< (real 1) (real 2))
            (car x)
            (car (cdr x))))))

   (equal? #t (fol-optimize '#t))
   (equal? #f (fol-optimize '#f))

   ;; Elimination should not get confused by procedures that return
   ;; multiple things only one of which is needed.
   (check-program-types
    (eliminate-intraprocedural-dead-variables
     '(begin
        (define (foo)
          (argument-types (values real real))
          (values 1 2))
        (let-values (((x y) (foo)))
          y))))

   (equal? #f (procedure-definitions->program
               (program->procedure-definitions #f)))

   ;; Interprocedural-dead-code-elimination should catch loops that
   ;; carry but do not use variables.
   (equal?
    '(begin
       (define (fact n)
         (argument-types real real)
         (if (= n 0)
             1
             (* n (fact (- n 1)))))
       (fact (real 5)))
    (interprocedural-dead-code-elimination
     '(begin
        (define (fact dead n)
          (argument-types real real real)
          (if (= n 0)
              1
              (* n (fact dead (- n 1)))))
        (fact (real 1) (real 5)))))

   ;; Interprocedural-dead-code-elimination should catch loops that
   ;; carry but do not use returns.
   (equal?
    '(begin
       (define (fact n)
         (argument-types real real)
         (if (= n 0)
             1
             (* n (fact (- n 1)))))
       (fact (real 5)))
    (reverse-anf
     (interprocedural-dead-code-elimination
      '(begin
         (define (fact n)
           (argument-types real (values real real))
           (if (= n 0)
               (values 0 1)
               (let-values (((dead n-1!) (fact (- n 1))))
                 (values
                  dead
                  (* n n-1!)))))
         (let-values (((dead n!) (fact (real 5))))
           n!)))))

   ))
