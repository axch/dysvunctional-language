(declare (usual-integrations))
(in-test-group
 vl

 (define-each-check
   (equal? '((lambda (x) 1) 3)
	   (macroexpand '((lambda (x) 1) 3)))
   (equal? '(lambda ((cons x y)) x)
	   (macroexpand '(lambda ((cons x y)) x)))

   (equal? '((lambda ((cons x y)) x) (cons 1 2))
	   (macroexpand '((lambda (x y) x) (cons 1 2))))
   (equal? 1 (determined-answer '((lambda (x y) x) (cons 1 2))))
   (equal? 2 (determined-answer '((lambda (x y) y) (cons 1 2))))

   (equal? '((lambda ((cons x y)) x) (cons 1 2))
	   (macroexpand '((lambda (x y) x) 1 2)))
   (equal? 1 (determined-answer '((lambda (x y) x) 1 2)))
   (equal? 2 (determined-answer '((lambda (x y) y) 1 2)))

   (equal? '((lambda ((cons x (cons y z))) x) (cons 1 (cons 2 3)))
	   (macroexpand '((lambda (x y z) x) 1 2 3)))
   (equal? 1 (determined-answer '((lambda (x y z) x) 1 2 3)))
   (equal? 2 (determined-answer '((lambda (x y z) y) 1 2 3)))
   (equal? 3 (determined-answer '((lambda (x y z) z) 1 2 3)))

   (equal? '((lambda ((cons (cons x y) z)) x) (cons (cons 1 2) 3))
	   (macroexpand '((lambda ((cons x y) z) x) (cons 1 2) 3)))
   (equal? 1 (determined-answer '((lambda ((cons x y) z) x) (cons 1 2) 3)))
   (equal? 2 (determined-answer '((lambda ((cons x y) z) y) (cons 1 2) 3)))
   (equal? 3 (determined-answer '((lambda ((cons x y) z) z) (cons 1 2) 3)))

   (equal? '((lambda ((cons (cons x (cons y ())) z)) x)
	     (cons (cons 1 (cons 2 ())) 3))
	   (macroexpand '((lambda ((x y) z) x)
			  (cons 1 (cons 2 ())) 3)))
   (equal? 1 (determined-answer
              '((lambda ((x y) z) x)
                (cons 1 (cons 2 ())) 3)))
   (equal? 2 (determined-answer
              '((lambda ((x y) z) y)
                (cons 1 (cons 2 ())) 3)))
   (equal? 3 (determined-answer
              '((lambda ((x y) z) z)
                (cons 1 (cons 2 ())) 3)))

   (equal? '((lambda (()) 1) ())
	   (macroexpand '((lambda () 1))))
   (equal? 1 (determined-answer '((lambda () 1))))

   (equal? 3 (interpret 3))
   (equal? 1 (interpret '((lambda (x) 1) 3)))
   (equal? 3 (interpret '((lambda (x y) 3) 1 2)))
   (equal? 3 (interpret '((lambda ((cons x y)) 3) (cons 1 2))))
   (equal? 2 (interpret '((lambda ((cons x y)) y) (cons 1 2))))
   (equal? '(1 . 3) (interpret '((lambda (x (y . z) w)
				 (cons x z))
			       1 (cons 2 3) 4)))
   (equal? 4 (interpret '(+ 1 3)))
   (equal? 3 (interpret '(let ((x 3)) x)))
   (equal? '(8 . 16)
	   (interpret '(let ((double (lambda (x)
				     (+ x x)))
			   (square (lambda (x)
				     (* x x)))
			   (compose (lambda (f g)
				      (lambda (x) (f (g x))))))
		       (cons ((compose double square) 2)
			     ((compose square double) 2)))))

   (equal? '(3 . 2) (analyzed-answer '(cons 3 2)))
   (equal? 2 (analyzed-answer '((lambda (x) 2) 3)))
   (equal? 2 (analyzed-answer '((lambda (x y) 2) 3 4)))
   (equal? 3 (analyzed-answer '((lambda (x y) x) 3 4)))
   (equal? 4 (analyzed-answer '((lambda (x y) y) 3 4)))
   (equal? '(8 . 16)
	   (analyzed-answer
	    '(let ((double (lambda (x)
			     (+ x x)))
		   (square (lambda (x)
			     (* x x)))
		   (compose (lambda (f g)
			      (lambda (x) (f (g x))))))
	       (cons ((compose double square) 2)
		     ((compose square double) 2)))))
   (equal? `(,abstract-real . ,abstract-real)
    (analyzed-answer
     '(let ((double (lambda (x)
		      (+ x x)))
	    (square (lambda (x)
		      (* x x)))
	    (compose (lambda (f g)
		       (lambda (x) (f (g x))))))
	(cons ((compose double square) (real 2))
	      ((compose square double) (real 2))))))
   (equal? '(8 . 16)
    (union-free-answer
     '(let ((double (lambda (x)
		      (+ x x)))
	    (square (lambda (x)
		      (* x x)))
	    (compose (lambda (f g)
		       (lambda (x) (f (g x))))))
	(cons ((compose double square) (real 2))
	      ((compose square double) (real 2))))))
   (equal? 8
    (union-free-answer
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn (real 5))))
	  (add5 (real 3))))))
   (equal? 8
    (union-free-answer
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn (real 5))))
	  (add5 3)))))
   (equal? 8
    (union-free-answer
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn 5)))
	  (add5 (real 3))))))
   (equal? 27
    (union-free-answer
     '(let ((cube (lambda (x)
		    (* x (* x x)))))
	(let ((enlarge-upto (lambda (bound)
			      (lambda (x)
				(if (< x bound)
				    (cube x)
				    x)))))
	  ((enlarge-upto (real 20)) (real 3))))))
   (equal? 1
    (union-free-answer
     '(letrec ((fact (lambda (n)
		       (if (= n 1)
			   1
			   (* n (fact (- n 1)))))))
	(fact 1))))
   (equal? 120
    (union-free-answer
     '(letrec ((fact (lambda (n)
		       (if (= n 1)
			   1
			   (* n (fact (- n 1)))))))
	(fact (real 5)))))
   (equal? 10
    (union-free-answer
     '(let loop ((count (real 0)))
	(if (< count 10)
	    (loop (+ count 1))
	    count))))
   (equal? #f (union-free-answer '(pair? (real 3))))
   (equal? #t (union-free-answer '(real? (real 3))))
   (equal? #f (union-free-answer '(zero? (real 3))))
   (equal? #t (union-free-answer '(positive? (real 3))))
   (equal? #f (union-free-answer '(negative? (real 3))))
   )

 ;; VL is too slow and too not-under-active-development to run these,
 ;; especially since dvl-test.scm runs the same programs under DVL
 ;; anyway.
; (for-each-example "../examples/small.vl" define-union-free-example-test)
; (for-each-example "test-vl-programs.scm" define-union-free-example-test)
#;
 (define-test (executable-entry-point)
   (check
    (equal?
     "(-9.223456610994083e-3 . 1.0078886869645214)\n"
     (with-output-to-string
       (lambda ()
         (vl-run-file "examples/euler-integral.vl"))))))
 )
