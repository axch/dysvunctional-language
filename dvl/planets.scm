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

(naive-euler initial-state state-derivative step-size (real 10000))

#|
On khazad-dum, Apr 4, 2011:
(define tidied (compile-visibly (dvl-read-file "planets.scm")))
Stage analyze-and-generate on 2731 pairs
;process time: 45890 (22480 RUN + 23410 GC); real time: 48822
Stage structure-definitions->vectors on 801317 pairs
;process time: 4540 (1500 RUN + 3040 GC); real time: 4551
Stage inline on 799903 pairs
;process time: 790 (560 RUN + 230 GC); real time: 781
Stage scalar-replace-aggregates on 103805 pairs
;process time: 4490 (1360 RUN + 3130 GC); real time: 4501
Stage intraprocedural-de-alias on 1264796 pairs
;process time: 12940 (1590 RUN + 11350 GC); real time: 15904
Stage eliminate-intraprocedural-dead-variables on 14650 pairs
;process time: 12370 (12280 RUN + 90 GC); real time: 20852
Stage tidy on 8489 pairs
;process time: 680 (440 RUN + 240 GC); real time: 689
;Value: tidied
|#
