(in-test-group
 vl

 (define-each-check
   (equal? '(cons 1 2) (replace-free-occurrences 'foo 'bar '(cons 1 2)))
   (equal? '(let ((x y)) 1) (replace-free-occurrences 'foo 'bar '(let ((x y)) 1)))
   (equal? '(let ((x bar)) 1) (replace-free-occurrences 'foo 'bar '(let ((x foo)) 1)))
   (equal? '((cons 1 2)) (replace-free-occurrences 'foo 'bar '((cons 1 2))))
   (equal? '(lambda () (cons 1 2)) (replace-free-occurrences 'foo 'bar '(lambda () (cons 1 2))))
   )

 (define-each-check
   (equal? '((lambda (x) 1) 3)
	   (macroexpand '((lambda (x) 1) 3)))
   (equal? '(lambda ((cons x y)) x)
	   (macroexpand '(lambda ((cons x y)) x)))

   (equal? '((lambda ((cons x y)) x) (cons 1 2))
	   (macroexpand '((lambda (x y) x) (cons 1 2))))
   (not (determined-form-breakage 1 '((lambda (x y) x) (cons 1 2))))
   (not (determined-form-breakage 2 '((lambda (x y) y) (cons 1 2))))

   (equal? '((lambda ((cons x y)) x) (cons 1 2))
	   (macroexpand '((lambda (x y) x) 1 2)))
   (not (determined-form-breakage 1 '((lambda (x y) x) 1 2)))
   (not (determined-form-breakage 2 '((lambda (x y) y) 1 2)))

   (equal? '((lambda ((cons x (cons y z))) x) (cons 1 (cons 2 3)))
	   (macroexpand '((lambda (x y z) x) 1 2 3)))
   (not (determined-form-breakage 1 '((lambda (x y z) x) 1 2 3)))
   (not (determined-form-breakage 2 '((lambda (x y z) y) 1 2 3)))
   (not (determined-form-breakage 3 '((lambda (x y z) z) 1 2 3)))

   (equal? '((lambda ((cons (cons x y) z)) x) (cons (cons 1 2) 3))
	   (macroexpand '((lambda ((cons x y) z) x) (cons 1 2) 3)))
   (not (determined-form-breakage 1 '((lambda ((cons x y) z) x) (cons 1 2) 3)))
   (not (determined-form-breakage 2 '((lambda ((cons x y) z) y) (cons 1 2) 3)))
   (not (determined-form-breakage 3 '((lambda ((cons x y) z) z) (cons 1 2) 3)))

   (equal? '((lambda ((cons (cons x (cons y ())) z)) x)
	     (cons (cons 1 (cons 2 ())) 3))
	   (macroexpand '((lambda ((x y) z) x)
			  (cons 1 (cons 2 ())) 3)))
   (not (determined-form-breakage 1
         '((lambda ((x y) z) x)
	   (cons 1 (cons 2 ())) 3)))
   (not (determined-form-breakage 2
         '((lambda ((x y) z) y)
	   (cons 1 (cons 2 ())) 3)))
   (not (determined-form-breakage 3
         '((lambda ((x y) z) z)
	   (cons 1 (cons 2 ())) 3)))

   (equal? '((lambda (()) 1) ())
	   (macroexpand '((lambda () 1))))
   (not (determined-form-breakage 1 '((lambda () 1))))

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
    (eval-through-scheme
     '(let ((double (lambda (x)
		      (+ x x)))
	    (square (lambda (x)
		      (* x x)))
	    (compose (lambda (f g)
		       (lambda (x) (f (g x))))))
	(cons ((compose double square) (real 2))
	      ((compose square double) (real 2))))))
   (equal? 8
    (eval-through-scheme
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn (real 5))))
	  (add5 (real 3))))))
   (equal? 8
    (eval-through-scheme
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn (real 5))))
	  (add5 3)))))
   (equal? 8
    (eval-through-scheme
     '(let ((addn (lambda (n) (lambda (x) (+ n x)))))
	(let ((add5 (addn 5)))
	  (add5 (real 3))))))
   (equal? 27
    (eval-through-scheme
     '(let ((cube (lambda (x)
		    (* x (* x x)))))
	(let ((enlarge-upto (lambda (bound)
			      (lambda (x)
				(if (< x bound)
				    (cube x)
				    x)))))
	  ((enlarge-upto (real 20)) (real 3))))))
   (equal? 1
    (eval-through-scheme
     '(letrec ((fact (lambda (n)
		       (if (= n 1)
			   1
			   (* n (fact (- n 1)))))))
	(fact 1))))
   (equal? 120
    (eval-through-scheme
     '(letrec ((fact (lambda (n)
		       (if (= n 1)
			   1
			   (* n (fact (- n 1)))))))
	(fact (real 5)))))
   (equal? 10
    (eval-through-scheme
     '(let loop ((count (real 0)))
	(if (< count 10)
	    (loop (+ count 1))
	    count))))
   (equal? #f (eval-through-scheme '(pair? (real 3))))
   (equal? #t (eval-through-scheme '(real? (real 3))))
   (equal? #f (eval-through-scheme '(zero? (real 3))))
   (equal? #t (eval-through-scheme '(positive? (real 3))))
   (equal? #f (eval-through-scheme '(negative? (real 3))))
   )

 (with-input-from-file "../examples.scm"
   (lambda ()
     (let loop ((program (read)))
       (if (not (eof-object? program))
	   (begin (define-test
		    ;; Check that interpret and compile-to-scheme agree
		    (eval-through-scheme program))
		  (loop (read)))))))

 (with-input-from-file "test-vl-programs.scm"
   (lambda ()
     (let loop ((program (read)))
       (if (not (eof-object? program))
	   (begin (define-test
		    ;; Check that interpret and compile-to-scheme agree
		    (eval-through-scheme program))
		  (loop (read)))))))
 )
