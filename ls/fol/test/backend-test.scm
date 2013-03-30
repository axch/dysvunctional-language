(declare (usual-integrations))
(in-test-group
 backend

 (define factorial
   '(begin
      (define (fact n)
        (argument-types real real)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
      (fact 4)))

 (define-each-check
   (equal? '()
           (prepare-for-common-lisp factorial)))
 )
