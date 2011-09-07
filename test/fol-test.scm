(declare (usual-integrations))

;;; Utils

(define (define-external-fol-test dvl-program)
  (define-test
    (let* ((fol-program                 (compile-to-raw-fol dvl-program))
           (optimized-fol-program       (external-fol-optimize fol-program))
           (fol-program-value           (fol-eval fol-program))
           (optimized-fol-program-value (fol-eval optimized-fol-program)))
      (check (equal? fol-program-value optimized-fol-program-value)))))

(define *fol-executable* "/home/manzyuk/Projects/FOL/fol")

(define (pipe-through command input)
  (with-output-to-string
   (lambda ()
     (with-input-from-string input
       (lambda ()
         (run-shell-command command
                            'input  (current-input-port)
                            'output (current-output-port)))))))

(define (write-to-string object)
  (with-output-to-string (lambda () (write object))))

(define (read-from-string string)
  (with-input-from-string string (lambda () (read))))

(define (external-fol-optimize fol-program)
  (read-from-string
   (pipe-through
    *fol-executable*
    (write-to-string fol-program))))

;;; Tests

(define-external-fol-test
  '(let ()
     (define (fact n)
       (if (= n 0)
           1
           (* n (fact (- n 1)))))
     (fact (real 4.))))

(define-external-fol-test
  '(let ((increment (lambda (x) (+ x 1)))
         (double (lambda (x) (* x 2)))
         (car (lambda ((cons x ())) x))
         (cdr (lambda ((cons () y)) y)))
     (letrec ((map (lambda (f lst)
                     (if (null? lst)
                         ()
                         (cons (f (car lst)) (map f (cdr lst)))))))
       (cons (map increment (real 1.) (real 2.) (real 3.) ())
             (map double (real 4.) (real 5.) ())))))

(define-external-fol-test
  '(let ()
     (define (heron-step x)
       (lambda (guess)
         (/ (+ guess (/ x guess)) 2)))

     (define (close-enuf? a b)
       (< (abs (- a b)) 0.00001))

     (define (numeric-fix f start close-enuf?)
       (let loop ((old start)
                  (new (f start)))
         (if (close-enuf? old new)
             new
             (loop new (f new)))))

     (define (square-root x)
       (numeric-fix (heron-step x) (real 1.0) close-enuf?))

     (cons (sqrt 2.) (square-root (real 2.)))))

(define-external-fol-test
  '(letrec ((even? (lambda (n)
                     (if (= n 0)
                         #t
                         (odd? (- n 1)))))
            (odd? (lambda (n)
                    (if (= n 0)
                        #f
                        (even? (- n 1))))))
     (even? (real 5.))))
