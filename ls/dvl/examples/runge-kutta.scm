;;; Load this after compiling runge-kutta.dvl with
;;; (fol->floating-mit-scheme (compile-visibly (dvl-read-file "examples/runge-kutta.dvl")))
;;; and feel free to draw some pictures.

(define integrators (run-mit-scheme))

(define euler-exp (car integrators))
(define euler-exp-2 (cadr integrators))
(define rk4-exp (caddr integrators))
(define rk4-exp-2 (cadddr integrators))

(define (show-1d-ode integrator time-step total-time)
  (let loop ((result '((0. . 1.)))
             (current-time 0.)
             (current-y 1.))
    (if (>= current-time total-time)
        (gnuplot-plot-alist (reverse result) "with points, exp(x)")
        (let ((one-step (((integrator current-time) current-y) time-step)))
          (let ((new-time (car one-step))
                (new-y (cdr one-step)))
            (loop (cons (cons new-time new-y) result)
                  new-time
                  new-y))))))
