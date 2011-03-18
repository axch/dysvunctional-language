(in-test-group
 post-processing

 (define-each-check
   (lset= eq? '(c a) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))
   (not (tidy-non-soundness
	 ;; Carelessly inlining y will change the scope of x
	 '(let ((x (vector 1 2)))
	    (cons x (cons x (let ((y (vector 3 x))
				  (x (vector 4)))
			      (cons x (cons x (vector-ref (vector-ref y 1) 1)))))))))

   (alpha-rename?
    '(begin
       (define (operation-2 n)
         (if (= n 1)
             1
             (* n (operation-2 (- n 1)))))
       (operation-2 (real 5)))
    (compile-to-scheme
     '(let ()
        (define (fact n)
          (if (= n 1)
              1
              (* n (fact (- n 1)))))
        (fact (real 5)))))

   (alpha-rename?
    '(cons
      (let ((x (* (real 2) (real 2))))
        (+ x x))
      (let ((x (+ (real 2) (real 2))))
        (* x x)))
    (compile-to-scheme
     '(let ((double (lambda (x) (+ x x)))
            (square (lambda (x) (* x x)))
            (compose (lambda (f g) (lambda (x) (f (g x))))))
        (cons ((compose double square) (real 2))
              ((compose square double) (real 2))))))

   ;; Note the extra variable the-closure-53, below.  This is a use
   ;; case for interprocedural alias elimination.
   (alpha-rename?
    '(begin
       (define (operation-9 the-closure-41
                            the-closure-53
                            the-formals-56
                            the-formals-57)
         (argument-types (the-closure-41 real)
                         (the-closure-53 real)
                         (the-formals-56 real)
                         (the-formals-57 real)
                         real)
         (if (< (abs (- the-formals-56 the-formals-57)) .00001)
             the-formals-57
             (operation-9
              the-closure-53
              the-closure-53
              the-formals-57
              (/ (+ the-formals-57 (/ the-closure-41 the-formals-57)) 2))))
       (cons
        1.4142135623730951
        (operation-9 (real 2)
                     (real 2)
                     (real 1.)
                     (/ (+ (real 1.) (/ (real 2) (real 1.))) 2))))
    (compile-to-scheme
     '(let ()
        (define (heron-step x)
          (lambda (guess)
            (/ (+ guess (/ x guess)) 2)))
        (define (close-enuf? a b)
          (< (abs (- a b)) .00001))
        (define (numeric-fix f start close-enuf?)
          (let loop ((old start) (new (f start)))
            (if (close-enuf? old new)
                new
                (loop new (f new)))))
        (define (square-root x)
          (numeric-fix (heron-step x) (real 1.) close-enuf?))
        (cons (sqrt 2) (square-root (real 2))))))

   (alpha-rename?
    '(if (< (real 1) (real 2))
         (real 0)
         (+ (real 1) (real 2)))
    (prettify-compiler-output
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
    (prettify-compiler-output
     '(car
       (let ((x (let ((y (+ (real 1) (real 2))))
                  (cons y (cons y (vector))))))
         (if (< (real 1) (real 2))
             (cons (real 0) (cons (real 1) (vector)))
             x)))))

   (alpha-rename?
    '(+ (real 1) (real 2))
    (prettify-compiler-output
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
    (prettify-compiler-output
     '(let ((x (let ((y (+ (real 1) (real 2))))
                 (cons (+ y (real 0)) (cons y (vector))))))
        (if (< (real 1) (real 2))
            (car x)
            (car (cdr x))))))))

;; Here is a case where serious SRA is necessary
;; (let ((x (if ... (cons) (cons))))
;;   (.... (car x) .. (cdr x)))
