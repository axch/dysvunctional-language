(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (nr-sqrt x)
  (letrec ((loop (lambda (y)
                   (let ((y-prime (- y (/ (- (* y y) x) (+ y y)))))
                     (if (<= (abs (- y y-prime)) 1e-5)
                         y
                         (loop y-prime))))))
    (loop (- (+ x 1.0) x))))

((derivative-using-j* nr-sqrt) 4)
===> 0.2500002594080783

; (define program (dvl-read-file "examples.scm"))
; (define done (with-stack-sampling 20 (lambda () (show-time (lambda () (compile-to-fol program))))))
