;;;; Example VL programs

;;; Adding 5 to 3

(let ((addn (lambda (n)
	      (lambda (x) (+ n x)))))
  (let ((add5 (addn (real 5))))
    (add5 (real 3))))

;;; Variations on IF

(if (< (real 3) (real 6))
    (real 4)
    (real 3))

(if (< 3 6)
    (real 4)
    (real 3))

;;; Doubling and squaring do not commute under composition

(let ((double (lambda (x) (+ x x)))
      (square (lambda (x) (* x x)))
      (compose (lambda (f g)
		 (lambda (x) (f (g x))))))
  (cons ((compose double square) (real 2))
	((compose square double) (real 2))))

;;; Cubing 3 if it's less than 20

(let ((cube (lambda (x) (* x (* x x)))))
  (let ((enlarge-upto (lambda (bound)
			(lambda (x)
			  (if (< x bound)
			      (cube x)
			      x)))))
    ((enlarge-upto (real 20)) (real 3))))

;;; Variations on destructuring

(let ((my-add (lambda (x y) (+ x y))))
  (my-add (real 3) (real 6)))

(let ((my-add (lambda (foo) (+ foo))))
  (my-add (cons (real 3) (real 6))))

(let ((my-add (lambda (foo) (+ foo))))
  (my-add (cons 3 (real 6))))

(let ((delay-add (lambda (x y) (lambda () (+ x y)))))
  ((delay-add (real 3) (real 6))))
;;; Factorial

(letrec ((fact (lambda (n)
		 (if (= n 1)
		     1
		     (* n (fact (- n 1)))))))
  (fact 5))

;;; Factorial, with letrec manually macro expanded

(let ((Z (lambda (f)
	   ((lambda (x)
	      (f (lambda (y) ((x x) y))))
	    (lambda (x)
	      (f (lambda (y) ((x x) y))))))))
  (let ((fact (Z (lambda (fact)
		   (lambda (n)
		     (if (= n 1)
			 1
			 (* n (fact (- n 1)))))))))
    (fact (real 5))))

;;; Parity testing

(letrec ((even? (lambda (n)
		  (if (= n 0)
		      #t
		      (odd? (- n 1)))))
	 (odd? (lambda (n)
		 (if (= n 0)
		     #f
		     (even? (- n 1))))))
  (even? (real 5)))

;;; Counting to 10

(let loop ((count (real 0)))
  (if (< count 10)
      (loop (+ count 1))
      count))

;;; Mapping different functions over different length lists.

(let ((increment (lambda (x) (+ x 1)))
      (double (lambda (x) (* x 2)))
      (car (lambda ((cons x ())) x))
      (cdr (lambda ((cons () y)) y)))
  (letrec ((map (lambda (f lst)
		  (if (null? lst)
		      ()
		      (cons (f (car lst)) (map f (cdr lst)))))))
    (cons (map increment (real 1) (real 2) (real 3) ())
	  (map double (real 4) (real 5) ()))))

;;; Vector addition

(let ((car (lambda ((cons x ())) x))
      (cdr (lambda ((cons () y)) y)))
  (define (v+ structure1 structure2)
    (cond ((and (pair? structure1)
		(pair? structure2))
	   (cons (v+ (car structure1)
		     (car structure2))
		 (v+ (cdr structure1)
		     (cdr structure2))))
	  ((and (null? structure1) (null? structure2))
	   ())
	  (#t	     ; (and (number? structure1) (number? structure2))
	   (+ structure1 structure2))))
  (cons (v+ (real 1) (real 2))
	(v+ (cons (real 10) (real 20))
	    (cons (real 1) (real 2)))))
