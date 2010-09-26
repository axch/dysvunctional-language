(define (analyzed-answer program)
  (let ((candidate
	 (assoc (macroexpand program)
		(analysis-bindings (analyze program)))))
    (if (not candidate)
	(error "Analysis makes no binding for the original program"
	       program)
	(caddr candidate))))

(define (eval-through-scheme program)
  (eval (compile-to-scheme program) (nearest-repl/environment)))

(define (determined-form-breakage value form)
  (cond ((not (equal? (macroexpand form) (macroexpand (macroexpand form))))
	 `(not (equal? ,(macroexpand form) ,(macroexpand (macroexpand form)))))
	((not (equal? value (vl-eval form)))
	 `(not (equal? ,value (interpreted ,(vl-eval form)))))
	((not (equal? value (analyzed-answer form)))
	 `(not (equal? ,value (analyzed ,(analyzed-answer form)))))
	((not (equal? `(begin ,value) (compile-to-scheme form)))
	 `(not (equal? ,value (compiled ,(compile-to-scheme form)))))
	(else #f)))

(in-test-group
 vl

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

   (equal? 3 (vl-eval 3))
   (equal? 1 (vl-eval '((lambda (x) 1) 3)))
   (equal? 3 (vl-eval '((lambda (x y) 3) 1 2)))
   (equal? 3 (vl-eval '((lambda ((cons x y)) 3) (cons 1 2))))
   (equal? 2 (vl-eval '((lambda ((cons x y)) y) (cons 1 2))))
   (equal? '(1 . 3) (vl-eval '((lambda (x (y . z) w)
				 (cons x z))
			       1 (cons 2 3) 4)))
   (equal? 4 (vl-eval '(+ 1 3)))
   (equal? 3 (vl-eval '(let ((x 3)) x)))
   (equal? '(8 . 16)
	   (vl-eval '(let ((double (lambda (x)
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
   (equal? '((abstract-real) . (abstract-real))
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
   ))
