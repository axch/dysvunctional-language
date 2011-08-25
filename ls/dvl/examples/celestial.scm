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

(define win (frame -6 6 -6 6))

(define (stream-take count stream)
  (if (= count 0)
      stream
      (stream-take (- count 1) (force (cdr stream)))))

;; Compile the DVL program with
(fol->mit-scheme (compile-visibly (dvl-read-file "examples/celestial.dvl")))

;; Then run with
(stream-take
 1000
 (stream-for-each
  (plot-objects window)
  ((constant-arg-for-dvl-stream 10.)
   (run-mit-scheme))))
