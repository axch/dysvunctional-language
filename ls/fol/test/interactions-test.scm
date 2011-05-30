(in-test-group
 interactions

 (define-test (sra-then-inline)
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
     (inline (scalar-replace-aggregates program))))))
