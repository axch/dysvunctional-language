;;; Load this after compiling runge-kutta.dvl with
;;; (fol->floating-mit-scheme (compile-to-fol (dvl-source "examples/runge-kutta.dvl") visibly))
;;; and feel free to draw some pictures.

(define integrators (run-mit-scheme))

(define euler (car integrators))
(define rk4  (cadr integrators))
#;
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


(define ((show-1d-ode thing-to-show) integrator time-step total-time)
  (let loop ((result '())
             (integrator integrator))
    (let* ((state (car integrator))
           (current-time (car state))
           (func (cdr integrator)))
      (if (>= current-time total-time)
          (gnuplot-plot-alist (map (lambda (datum)
                                     (let ((time (car datum))
                                           (estimate (cadr datum)))
                                       (cons time (thing-to-show time estimate))))
                                   (reverse result))
                              "with points, sin(x)")
          (loop (cons state result)
                (func time-step))))))

(define ((relative-error solution) time estimate)
   (if (>= (abs (solution time)) 1)
       (/ (- (solution time) estimate) (solution time))
       (- (solution time) estimate)))

(define (answer time estimate) estimate)

(define show-1d (show-1d-ode answer))
