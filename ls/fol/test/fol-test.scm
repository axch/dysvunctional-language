(declare (usual-integrations))
(in-test-group
 fol

 (define-each-check

   ;; Alpha renaming needs to respect the scope of procedure
   ;; definitions properly.
   (alpha-rename?
    '(begin
       (define (fact n)
         (if (= n 0)
             1
             (* n (- (fact n 1)))))
       (fact 4))
    '(begin
       (define (fiction m)
         (if (= m 0)
             1
             (* m (- (fiction m 1)))))
       (fiction 4)))

   (equal?
    '(vector-ref (vector 1 2) 0)
    (structure-definitions->vectors
     '(begin
        (define-typed-structure foo (bar real) (baz real))
        (foo-bar (make-foo 1 2)))))

   (lset= eq? '(c a) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))

   (lset= eq? '(a c d)
          (feedback-vertex-set
           ;; This is G from the comments in feedback-vertex-set.scm
           '((a b) (b a c) (c a e) (d c e) (e d))))

   (equal?
    '(let ((x y))
       x)
    (lift-lets-expression
     '(let ((x y))
        x)))

   (equal?
    '(let ((y z))
       (let ((x y))
         x))
    (lift-lets-expression
     '(let ((x (let ((y z)) y)))
        x)))

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
    (%eliminate-intraprocedural-dead-code
     '(begin
        (define (foo)
          (argument-types (values real real))
          (values 1 2))
        (let-values (((x y) (foo)))
          y))))

   (equal?
    '(lambda (x) (+ x 1))
    (%eliminate-intraprocedural-dead-code
     '(let ((x (real 3)))
        (lambda (x)
          (+ x 1)))))

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

   ;; Dead variable elimination should emit syntactically correct
   ;; stuff, no?
   (equal?
    '(begin
       (define (foo)
         (argument-types real)
         (let ((dead ()))
           1))
       (foo))
    (%interprocedural-dead-code-elimination
     '(begin
        (define (foo dead)
          (argument-types real real)
          1)
        (foo 5))))

   ;; Elimination should keep all inputs that are needed by any
   ;; output, even if some output does not need them.
   (equal?
    '(begin
       (define (foo not-dead)
         (argument-types real (values real real))
         (values not-dead 1))
       (let-values (((x one) (foo 5)))
         (cons x one)))
    (interprocedural-dead-code-elimination
     '(begin
        (define (foo not-dead)
          (argument-types real (values real real))
          (values not-dead 1))
        (let-values (((x one) (foo 5)))
          (cons x one)))))

   ;; Do not inline bindings into lambdas
   (equal?
    '(let ((x (real 3)))
       (lambda (foo)
         (+ foo x)))
    (reverse-anf
     '(let ((x (real 3)))
        (lambda (foo)
          (+ foo x)))))

   ;; Dead code elimination should not barf on unused procedures.
   (equal?
    '(begin (define (fact) (argument-types (values)) (values)) 1)
    (interprocedural-dead-code-elimination
     '(begin
        (define (fact n)
          (argument-types real real)
          (if (= n 1)
              1
              (* n (fact (- n 1)))))
        1)))

   ;; TODO The fol optimizer should get rid of unused procedures, even
   ;; if they call themselves.
   #;
   (equal? 1 (fol-optimize
              '(begin
                 (define (fact n)
                   (argument-types real real)
                   (if (= n 1)
                       1
                       (* n (fact (- n 1)))))
                 1)))
   )

 (define-test (stages-should-not-change-annotations-on-their-input)
   (define program '(begin
                      (define (fact n)
                        (argument-types real real)
                        (if (= n 1)
                            1
                            (* n (fact (- n 1)))))
                      (fact 5)))
   (present! 'lets-lifted program)
   (present! 'unique-names program)
   (for-each (lambda (stage)
               (stage program)
               (check (equal? '((unique-names . #t) (lets-lifted . #t))
                              (hash-table/get eq-properties program #f))))
             (list inline intraprocedural-cse
                   eliminate-intraprocedural-dead-code
                   eliminate-interprocedural-dead-code
                   scalar-replace-aggregates reverse-anf)))

 )
