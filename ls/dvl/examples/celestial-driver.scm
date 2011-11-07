(declare (usual-integrations))

(define (constant-arg-for-dvl-stream argument)
  (lambda (dvl-stream)
    (let loop ((dvl-stream dvl-stream))
      (cons (car dvl-stream)
            (delay (loop ((cdr dvl-stream) argument)))))))

(define (stream-for-each f stream)
  (cons (f (car stream))
        (delay (stream-for-each f (force (cdr stream))))))

(define ((plot-object window) object)
  (let ((position (cadr object)))
    (let ((x (car position))
          (y (cadr position)))
      ;(pp `(object-at ,x ,y ,z))
      ;(pp `(speed ,@(caddr object)))
      ; Drop z coord in plot
      (plot-point window x y))))

(define ((plot-objects window) state)
  (for-each (plot-object window) (cdr state)))

(define window (frame -60 60 -60 60))

(define (stream-take count stream)
  (if (= count 0)
      stream
      (stream-take (- count 1) (force (cdr stream)))))

(define (compare state1 state2)
  (cond ((pair? state1)
         (cons (compare (car state1) (car state2))
               (compare (cdr state1) (cdr state2))))
        ((null? state1)
         '())
        (else (/ (- state1 state2) state1))))

;; Compile the DVL program with
;(fol->mit-scheme (compile-to-fol (dvl-read-file "examples/celestial.dvl") visibly))

;; Then run with
#;
(stream-take
 1000
 (stream-for-each
  (plot-objects window)
  ((constant-arg-for-dvl-stream 10.)
   ((run-mit-scheme) 100.))))
