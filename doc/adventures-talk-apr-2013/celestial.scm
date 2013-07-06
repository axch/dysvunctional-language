(define (potential-function objects)
  (lambda (positions)
    (potential
     (map update-position objects positions))))

(define (forces objects)
  (* -1 (gradient (potential-function objects)
                  (map position objects))))

(define (state-derivative state)
  (let ((time (car state))
        (objects (cdr state)))
    (cons 1 ; d(time) = 1
     (map make-object
          ;; d(masses) = 0
          (map (lambda (obj) 0) objects)
          ;; d(positions) = velocities
          (map velocity objects)
          ;; d(velocities) = forces / masses
          (map / (forces objects)
               (map mass objects))))))

(downsampled-stream
 (step-stream
  rk4
  state-derivative
  (cons 0 (list sun jupiter saturn
                uranus neptune)))
 10000)
