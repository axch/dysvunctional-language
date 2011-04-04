(include "math")

(define (pairwise-sum f lst)
  (cond ((null? lst)
         0)
        ((null? (cdr lst))
         0)
        (else
         (+ (pairwise-sum f (cdr lst))
            ((reduce + 0) (map (lambda (second)
                                (f (car lst) second))
                              (cdr lst)))))))

(define (distance pos1 pos2)
  (magnitude (v- pos2 pos1)))

(define (make-obj mass pos)
  (cons mass pos))
(define (mass obj) (car obj))
(define (position obj) (cdr obj))

(define (potential objects)
  (* G
     (pairwise-sum
      (lambda (obj1 obj2)
        (- 0
           (/ (* (mass obj1) (mass obj2))
              (distance (position obj1) (position obj2)))))
      objects)))

(define G (real 6.67300e-11))                ; m^3 / kg s^2
(define sun-mass (real 1.9891e30))           ; kg
(define sun (make-obj sun-mass (list 0 0)))
(define earth-mass (real 5.9742e24))         ; kg

(define earth-aphelion (real 152098232e3))   ; m

(define earth-avg-orbital-speed (real 29.78e3)) ; m/s

; A state is the position and velocity of the earth.

; We'll start the earth at aphelion, going perpendiculraly at its
; average orbital speed (Yes, I know this is wrong).
(define initial-state (list earth-aphelion (real 0) (real 0) earth-avg-orbital-speed))

(define (state-derivative (list earth-x earth-y earth-vx earth-vy))
  (let (((list earth-ax earth-ay)
         (k*v (/ 1 earth-mass)
              ((gradient-f (lambda ((list earth-x earth-y))
                             (potential (list sun (make-obj earth-mass (list earth-x earth-y))))))
               (list earth-x earth-y)))))
    (list earth-vx earth-vy earth-ax earth-ay)))

(define (naive-euler state state-deriv step count)
  (let loop ((state state)
             (count count))
    (if (<= count 0)
        state
        (loop
         (v+ state (k*v step (state-deriv state)))
         (- count 1)))))

(define step-size (real 20000))              ; s

(naive-euler initial-state state-derivative step-size (real 10000))
