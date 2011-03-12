;;; Variations on IF

(if (< (real 3) (real 6))
    (real 4)
    (real 3)) ===> 4

(if (< 3 6)
    (real 4)
    (real 3)) ===> 4

;;; Variations on destructuring

(let ((my-add (lambda (x y) (+ (real x) (real y)))))
  (my-add 3 6)) ===> 9

(let ((my-add (lambda (foo) (+ foo))))
  (my-add (cons (real 3) (real 6)))) ===> 9

(let ((my-add (lambda (foo) (+ foo))))
  (my-add (cons 3 (real 6)))) ===> 9

(let ((my-add (lambda (foo) (real (+ foo)))))
  (my-add 3 6)) ===> 9

(let ((delay-add (lambda (x y) (lambda () (+ x y)))))
  ((delay-add (real 3) (real 6)))) ===> 9

(let ((frobnicate (lambda (x) (real x))))
  (frobnicate 3)) ===> 3

;;; Recursion with non-primitive procedures

(let ((my-* (lambda (x y) (* x y))))
  (letrec ((fact (lambda (n)
		   (if (= n 1)
		       1
		       (my-* n (fact (- n 1)))))))
    (fact (real 5)))) ===> 120

(let ((my-* (lambda (x y) (* x y))))
  (letrec ((fact (lambda (n)
		   (if (= n 1)
		       1
		       (my-* n (fact (- n 1)))))))
    (fact 5))) ===> 120

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
    (fact (real 5)))) ===> 120

