(define (car (cons x ()))
  x)

(define (cdr (cons () y))
  y)

(define (v+ structure1 structure2)
  (cond ((and (pair? structure1)
	      (pair? structure2))
	 (cons (v+ (car structure1)
		   (car structure2))
	       (v+ (cdr structure1)
		   (cdr structure2))))
	((and (null? structure1) (null? structure2))
	 ())
	(#t ; (and (number? structure1) (number? structure2))
	 (+ structure1 structure2))))

(define (k*v number structure)
  (cond ((pair? structure)
	 (cons (k*v number (car structure))
	       (k*v number (cdr structure))))
	((null? structure)
	 ())
	(#t ; (number? structure)
	 (* number structure))))

(define (naive-euler state-derivative initial-state stop? step)
  (let loop ((state initial-state))
    (if (stop? state)
	state
	(loop (v+ state (k*v step (state-derivative state)))))))

(define (circle-field state)
  (cons (* (real -1) (cdr state)) (car state)))

(define (upper-left-quadrant? state)
  (< (car state) (real 0)))

(naive-euler circle-field (cons (real 1) (real 0)) upper-left-quadrant? (real 0.01))
