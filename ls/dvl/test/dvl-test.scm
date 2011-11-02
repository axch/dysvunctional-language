(declare (usual-integrations))

(define (in-and-loop expression)
  `(let ((x (gensym)))
     (let loop ((count (real 10))
                (answer #t))
       (if (= count 0)
           answer
           (loop (- count 1) (and answer ,expression))))))

(define (in-frobnicating-loop expression)
  `(let ((x (gensym)))
     (define (frobnicate symbol)
       (if (> (real 2) 1)
           symbol
           x))
     (let loop ((count (real 10))
                (answer #t))
       (if (= count 0)
           answer
           (loop (- count 1) (and answer ,expression))))))

(in-test-group
 dvl
 (define-each-check
   (equal? 3 (determined-answer '(+ 1 2)))
   (equal? #f (determined-answer '(gensym= (gensym) (gensym))))
   (equal? #t (determined-answer '(gensym< (gensym) (gensym))))
   (equal? #t (determined-answer '(let ((x (gensym))) (gensym= x x))))
   (equal? #f (determined-answer '(let ((x (gensym))) (gensym< x x))))
   (equal? #f (determined-answer '(let ((x (gensym))) (gensym= x (gensym)))))
   (equal? #t (determined-answer '(let ((x (gensym))) (gensym< x (gensym)))))
   (equal? #f (determined-answer '(let ((x (gensym))) (gensym< (gensym) x))))

   (equal? #t
    (union-free-answer
     '(let ((x (gensym)))
        (gensym= x (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     '(let ((x (gensym)))
        (gensym= x (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     '(let ((x (gensym)))
        (gensym< x (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #t
    (union-free-answer
     '(let ((x (gensym)))
        (gensym< x (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     '(let ((x (gensym)))
        ;; The analysis could solve this with more accurate modeling
        ;; of possible gensym values
        (gensym= (gensym) (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     '(let ((x (gensym)))
        (gensym< (gensym) (if (> (real 2) (real 1)) x (gensym))))))

   (equal?
    '(if (= (real 10) 0)
         #t
         #f)
    (compile-carefully
     (in-and-loop '(gensym= (gensym) (gensym)))))

   (equal? #t (compile-carefully
               (in-and-loop '(gensym< (gensym) (gensym)))))

   (equal? #t
    (loose-union-free-answer
     (in-and-loop '(gensym= x (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (loose-union-free-answer
     (in-and-loop '(gensym= x (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (loose-union-free-answer
     (in-and-loop '(gensym= (gensym) (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (loose-union-free-answer
     (in-and-loop '(gensym= (gensym) (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (loose-union-free-answer
     (in-and-loop '(gensym< x (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #t
    (loose-union-free-answer
     (in-and-loop '(gensym< x (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (loose-union-free-answer
     (in-and-loop '(gensym< (gensym) (if (> (real 2) (real 1)) x (gensym))))))

   ;; TODO These two break the analysis because it replicates the loop
   ;; body for every different gensym it might be passed.
   #;
   (let ((x (gensym)))
     (gensym= x
      (let loop ((count (real 10))
                 (y (gensym)))
        (if (= count 0)
            (if (< (real 2) 1)
                x
                y)
            (loop (- count 1) (gensym))))))
   #;
   (let ((x (gensym)))
     (gensym= x
      (let loop ((count (real 10))
                 (y (gensym)))
        (if (= count 0)
            (if (< (real 2) 1)
                x
                y)
            (loop (- count 1) (if (> (real 2) 1) (gensym) y))))))

   (equal? #f
    (loose-union-free-answer
     (in-frobnicating-loop '(gensym= x (frobnicate (gensym))))))

   (equal? #t
    (loose-union-free-answer ;; TODO Actually determined, except for sweeping out dead cruft
     (in-frobnicating-loop '(gensym= x (frobnicate x)))))

   (equal? #t
    (loose-union-free-answer
     (in-frobnicating-loop '(let ((y (gensym)))
                              (gensym= y (frobnicate y))))))

   (equal? #f
    (loose-union-free-answer
     (in-frobnicating-loop '(let ((y (gensym)))
                              (gensym= y (frobnicate (gensym)))))))

   (equal? '(if (= (real 10) 0)
                #t
                #f)
    (compile-carefully
     (in-frobnicating-loop '(let ((y (gensym)))
                              (let ((z (gensym)))
                                (gensym= z (frobnicate y)))))))

   (equal? #t
    (loose-union-free-answer
     (in-frobnicating-loop '(gensym< x (frobnicate (gensym))))))

   (equal? #f
    (loose-union-free-answer ;; TODO Actually determined, except for sweeping out dead cruft
     (in-frobnicating-loop '(gensym< x (frobnicate x)))))

   (equal? #f
    (loose-union-free-answer
     (in-frobnicating-loop '(let ((y (gensym)))
                              (gensym< y (frobnicate y))))))

   (equal? '(if (= (real 10) 0)
                #t
                #f)
    (compile-carefully
     (in-frobnicating-loop '(let ((y (gensym)))
                              (let ((z (gensym)))
                                (gensym< z (frobnicate y)))))))

   ;; Be sure to analyze the interior of the lambda, as the outside
   ;; world may call it.
   (equal? 8 (length (analysis-bindings (analyze '(lambda (x) (+ x 1))))))

   )

 (define-test (escape-smoke)
   (let ((proc (fol-eval
                (analyze-and-generate
                 '(lambda (x) (lambda (y) (+ x y)))))))
     (check (equal? 5 ((proc 4) 1)))))

 (define-test (function-escapes-and-is-called-locally)
   (let ((proc (fol-eval
                (analyze-and-generate
                 '(let ()
                    (define ((fact dead) n)
                      (if (= n 0)
                          1
                          (* n ((fact dead) (- n 1)))))
                    fact)))))
     (check (equal? 5040 ((proc 4) 7)))))

 (define-test (optimized-function-escapes-and-is-called-locally)
   (let ((proc (loose-union-free-answer
                '(let ()
                   (define ((fact dead) n)
                     (if (= n 0)
                         1
                         (* n ((fact dead) (- n 1)))))
                   fact))))
     (check (equal? 5040 ((proc 4) 7)))))

 (define-test (multiple-functions-escape)
   (let ((procs (loose-union-free-answer
                 '(cons (lambda (x) (+ 1 x))
                        (lambda (x) (* 2 x))))))
     (check (equal? 5 ((car procs) 4)))
     (check (equal? 6 ((cdr procs) 3)))))

 (define-test (loop-escapes-poor-mans-stream)
   (define program
     '(let loop ((state (real 0)))
        (cons state
              (lambda (step)
                (loop (+ state step))))))
   (let ((stream (loose-union-free-answer program)))
     (check (equal? 0 (car stream)))
     (let ((tail ((cdr stream) 4)))
       (check (equal? 4 (car tail)))
       (check (equal? 7 (car ((cdr tail) 3))))))
   (check (alpha-rename?
           '(begin
              (define (loop accum)
                (argument-types real escaping-function)
                (lambda (inc)
                  (let ((new-accum (+ accum inc)))
                    (cons new-accum (loop new-accum)))))
              (let ((start (real 0)))
                (cons start (loop start))))
           (compile-carefully program))))

 (define-test (same-function-escapes-and-is-used-internally)
   (define program
     '(let ()
        (define (foo x)
          (+ x 5))
        ;; Non-escaping binding hides the need to make an escaping binding
        (let ((bogon (foo (real 4))))
          foo)))
   (let ((proc (loose-union-free-answer program)))
     (check (equal? 7 (proc 2)))
     (check (equal? 0 (proc -5)))))

 (for-each-example "../../vl/examples.scm"
                   define-loose-union-free-example-test)
 (for-each-example "../../vl/test/test-vl-programs.scm"
                   define-loose-union-free-example-test)

 (define-test (tangent-of-function)
   (check (equal? 1 (loose-union-free-answer
                     (dvl-prepare
                      '(let ()
                         (define (adder n)
                           (lambda (x)
                             (g:+ x n)))
                         (((derivative adder) (real 3)) (real 4))))))))

 (for-each-example "../../slad/essential-examples.scm"
  (lambda (program #!optional value)
    (define-loose-union-free-example-test
      (dvl-prepare (vlad->dvl program)) value)))

 (for-each-example "../examples/reverse-examples.dvl"
  (lambda (program #!optional value)
    (define-loose-union-free-example-test
      (dvl-prepare (vlad->dvl program)) value)))

 (define-test (compile-reverse-mode)
   (check (equal?
           '(cos (real 0))
           (compile-to-fol (dvl-prepare (vlad->dvl '(gradient-r sin (real 0))))))))

;; TODO These are commented out because they are still slow (as of Sep
;; 12, 2011) and have rounding error disagreements.  They also use
;; large but finite amounts of stack space (e.g. --stack 2000 is ok).
#;
 (for-each
  (lambda (file)
    (for-each-example file
     (lambda (program #!optional value)
       (define-loose-union-free-example-test
         (dvl-prepare (vlad->dvl program)) value))))
  '("../examples/amazing-bug.dvl"
    "../examples/amazing-bug-2.dvl"
    "../examples/amazing-bug-3.dvl"
    "../examples/amazing-bug-4.dvl"
    "../examples/amazing-bug-5.dvl"
    "../examples/non-bug.dvl"
    ))

;; TODO Also too slow
#;
 (for-each-example "../examples/sqrt-again.dvl"
  (lambda (program #!optional value)
    (define-loose-union-free-example-test
      (dvl-prepare (vlad->dvl program)) value)))

 (define-test (executable-entry-point-from-vl)
   (check
    (equal?
     "(-9.223456610994083e-3 . 1.0078886869645214)\n"
     (with-output-to-string
       (lambda ()
         (fluid-let ((sf:noisy? #f)
                     (compiler:noisy? #f))
           (pp (dvl-run-file "../vl/euler-integral.scm"))))))))

;; TODO This one is just slow (2 seconds on moria on Sep 12, 2011)
#;
 (define-test (executable-entry-point)
   (check
    (equal?
     ".2500002594080783\n"
     (with-output-to-string
       (lambda ()
         (pp (dvl-run-file "examples/sqrt.dvl")))))))
 )
