(in-test-group
 interactions

 (define-test (sra-then-inline)
   ;; SRA does ANF conversion, which can create aliases that inlining
   ;; first would have avoided.
   (define program
     '(begin
        (define (inlinee x)
          (argument-types real real)
          (* x x))
        (inlinee (real 1))))
   (check
    (equal?
     '(let ((x (real 1)))
        (* x x))
     (scalar-replace-aggregates (inline program))))
   (check
    (alpha-rename?
     '(let ((anf-42 (real 1)))
        (let ((x anf-42))
          (* x x)))
     (inline (scalar-replace-aggregates program))))
   (check
    (alpha-rename?
     '(let ((x (real 1)))
        (* x x))
     (intraprocedural-cse
      (inline
       (scalar-replace-aggregates program))))))

 (define-test (sra-then-dead-code)
   (define program
     '(let ((x (* (real 2) 2)))
        (car (cons (real 1) x))))
   ;; Dead code cannot kill structure slots
   (check (equal? program
           (interprocedural-dead-code-elimination program)))
   ;; unless they are exposed to it by SRA
   (check (equal? '(real 1)
           (tidy
            (interprocedural-dead-code-elimination
             (scalar-replace-aggregates program))))))

 (define-test (dead-code-then-inline)
   ;; Eliminating dead code may open inlining opportunities by
   ;; deleting edges in the call graph.
   (define program
     '(begin
        (define (nominally-recursive x)
          (argument-types real real)
          (let ((foo (if (< x 0)
                         x
                         (nominally-recursive (- x 1)))))
            x))
        (nominally-recursive (real 5))))
   (check (equal? program (inline program)))
   (check (equal? '(let ((x (real 5))) x)
           (inline (interprocedural-dead-code-elimination program)))))

 )
