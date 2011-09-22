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

(define ((((naive-euler state-deriv) state) step) count)
  (let loop ((state state)
             (count count))
    (if (<= count 0)
        state
        (loop
         (v+ state (k*v step (state-deriv state)))
         (- count 1)))))

(define step-size (real 20000))              ; s

;; Sussman says that with Runge-Kutta, one can reasonably take one-day
;; steps for the outer solar system for a few thousand years before
;; the roundoff and truncation error start to get you (see
;; ~gjs/physics/solar/; planet.scm).

;; A billion years on the digital orrery (15 Mflop machine) took a few
;; months, with 32.7-day steps for outer solar system (four big
;; planets, hairy integrator).  That's 11 billion steps, at about 11
;; kflop per step.  I have a 2 Gflop (serial) machine, so I should be
;; able to do 180000 steps of that per second; so repeating the
;; digital orrery should take about 20 hours of compute, assuming
;; perfect serial utilization.

;((((naive-euler state-derivative) initial-state) step-size) (real 1000000))

((naive-euler state-derivative) initial-state)

#|
On moria, July 24, 2011:
(define opt-fol (compile-to-fol (dvl-read-file "examples/planets.scm") visibly))
Stage analyze-and-generate on 2905 pairs
;process time: 11110 (7200 RUN + 3910 GC); real time: 11118
Stage structure-definitions->vectors on 46377 pairs
;process time: 180 (180 RUN + 0 GC); real time: 183
Stage inline on 807744 pairs
;process time: 720 (520 RUN + 200 GC); real time: 719
Stage scalar-replace-aggregates on 95803 pairs
;process time: 3450 (1320 RUN + 2130 GC); real time: 3450
Stage intraprocedural-cse on 1518812 pairs
;process time: 44400 (5420 RUN + 38980 GC); real time: 44448
Stage eliminate-intraprocedural-dead-variables on 7015 pairs
;process time: 690 (690 RUN + 0 GC); real time: 697
Stage interprocedural-dead-code-elimination on 6965 pairs
;process time: 270 (160 RUN + 110 GC); real time: 263
Stage reverse-anf on 622 pairs
;process time: 10 (10 RUN + 0 GC); real time: 10
Final output has 394 pairs
;Value: opt-fol
|#

#|
On arrakis, July 24, 2011
(define opt-fol (compile-to-fol (dvl-read-file "examples/planets.scm") visibly))
Stage analyze-and-generate on 2905 pairs
;process time: 2980 (2830 RUN + 150 GC); real time: 2991
Stage structure-definitions->vectors on 46377 pairs
;process time: 90 (90 RUN + 0 GC); real time: 82
Stage inline on 807744 pairs
;process time: 240 (240 RUN + 0 GC); real time: 251
Stage scalar-replace-aggregates on 95582 pairs
;process time: 490 (490 RUN + 0 GC); real time: 495
Stage intraprocedural-cse on 1510545 pairs
;process time: 2500 (2150 RUN + 350 GC); real time: 2504
Stage eliminate-intraprocedural-dead-variables on 7011 pairs
;process time: 190 (190 RUN + 0 GC); real time: 195
Stage interprocedural-dead-code-elimination on 6961 pairs
;process time: 50 (50 RUN + 0 GC); real time: 60
Stage reverse-anf on 610 pairs
;process time: 0 (0 RUN + 0 GC); real time: 2
Final output has 322 pairs
;Value: opt-fol
|#
