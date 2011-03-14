;;; Identity function
((derivative (lambda (x) x)) 7) ===> 1

;;; Constant function
((derivative (lambda (x) 3)) 7) ===> 0

;;; Transform of +
((derivative (lambda (x) (+ x 1))) 7) ===> 1
((derivative (lambda (x) (+ x x))) 7) ===> 2

;;; Nested constant function (don't conflate the perturbations)
(let ()
  (define (one x)
    ((derivative (lambda (y) (+ x y))) 3))
  ((derivative one) 7)) ===> 0

;;; Another don't conflate the perturbations
(let ()
  (define (one x)
    ((derivative (lambda (y) (+ x y))) 3))
  ((derivative (lambda (x)
                 (* x (one x))))
   7)) ===> 1

;;; Don't confuse the perturbations
;; I'm not sure this would catch a thing that swapped the
;; perturbations with each other, but it at least might.
(let ()
  (define (one x)
    ((derivative (lambda (y) (+ x y))) 3))
  ((derivative (lambda (x)
                 (* x (one (* 2 x)))))
   7)) ===> 1

;;; Another don't confuse the perturbations.
((derivative
  (lambda (y)
    ((derivative
      (lambda (x)
        (* x (* x y))))
     (* y 3))))
 5) ===> 60
