;;; Complex arithmetic library
(define (c:+ z1 z2)
  (cons (+ (car z1) (car z2))
        (+ (cdr z1) (cdr z2))))

(define (c:* z1 z2)
  (cons (- (* (car z1) (car z2))
           (* (cdr z1) (cdr z2)))
        (+ (* (car z1) (cdr z2))
           (* (cdr z1) (car z2)))))

(define c:0 (cons 0 0))

(define (magnitude z)
  (sqrt (+ (* (car z) (car z))
           (* (cdr z) (cdr z)))))
;;; Iteration library
(define (iterate count f x)
  (if (<= count 0)
      x
      (iterate (- count 1) f (f x))))

;;; Mandelbrot set membership test
(define ((step c) z)
  (c:+ (c:* z z) c))

(define (mandelbrot? c)
  (< (magnitude (iterate 100 (step c) c:0)) 2))

;;; Example
(mandelbrot? (cons 0.5 0.7))
;;; Compiled (variables renamed for clarity)
(define (iteration count c-real c-imag
                   z-real z-imag)
  (if (<= count 0)
      (values z-real z-imag)
      (iteration (- count 1)
       c-real c-imag
       (+ (- (* z-real z-real)
             (* z-imag z-imag))
          c-real)
       (+ (+ (* z-real z-imag)
             (* z-imag z-real))
          c-imag))))
(let-values
    (((ans-real ans-imag)
      (iteration 100 .5 .7 0 0)))
  (< (sqrt
      (+ (* ans-real ans-real)
         (* ans-imag ans-imag)))
     2))
