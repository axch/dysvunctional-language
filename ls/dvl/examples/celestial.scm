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
      ; Drop z coord
      (plot-point window x y))))

(define ((plot-objects window) state)
  (for-each (plot-object window) (cadr state)))

(define win (frame -6 6 -6 6))

(stream-for-each
 (plot-objects window)
 ((constant-arg-for-dvl-stream 10.)
  (run-mit-scheme)))
