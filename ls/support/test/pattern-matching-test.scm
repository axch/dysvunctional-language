(declare (usual-integrations))

(declare (integrate-external "../pattern-matching"))

(define (test-pattern thing)
  (case* thing
         ((pair (pair a d) dd) (+ a d dd))
         ((pair a d) (+ a d))))

(define (my-test-pattern thing)
  (if (pair? thing)
      (let ((a (car thing)) (d (cdr thing)))
        (if (pair? a)
            (+ (car a) (cdr a) d)
            (+ a d)))))

(define (test-pattern2 thing)
  (case* thing
   ((pair _ (pair _ d :as subthing)) (+ d (car subthing)))
   (_ thing)))

(define (my-test-pattern2 thing)
  (if (pair? thing)
      (let ((d (cdr thing)))
        (if (pair? d)
            (+ (car d) (cdr d))
            thing))
      thing))

(define (test-pattern3 thing)
  (case* thing
   ((null) 'null)
   (pair => (lambda (a d) (* a d)))
   (_ 'other)))

(define (my-test-pattern3 thing)
  (cond ((null? thing) 'null)
        ((pair? thing) (* (car thing) (cdr thing)))
        (else 'other)))

(define (same-result f1 f2 input)
  (check (equal? (f1 input) (f2 input))))

(in-test-group
 pattern-matching

 (define-each-check
   (equal? 6 (test-pattern (cons (cons 1 2) 3)))
   (equal? 7 (test-pattern (cons 4 3)))
   (equal? unspecific (test-pattern 'foo))

   (equal? 9 (test-pattern2 (cons 4 (cons 3 6))))
   (equal? 'foo (test-pattern2 'foo))

   (equal? 'null (test-pattern3 '()))
   (equal? 'other (test-pattern3 'null))
   (equal? 12 (test-pattern3 (cons 4 3))))

 (define-test (evaluate-only-once)
   (let ((count 0))
     (check (equal? 1
                    (case* (begin (set! count (+ count 1))
                                  count)
                           ((pair _ _) 'pair)
                           ((null) 'null)
                           ((boolean :as bool) bool)
                           ((number :as num) num))))))

 (define-each-test
   (same-result test-pattern my-test-pattern (cons (cons 1 2) 3))
   (same-result test-pattern my-test-pattern (cons 4 3))
   (same-result test-pattern my-test-pattern 'foo)

   (same-result test-pattern2 my-test-pattern2 (cons (cons 1 2) 3))
   (same-result test-pattern2 my-test-pattern2 (cons 4 3))
   (same-result test-pattern2 my-test-pattern2 (cons 4 (cons 3 6)))
   (same-result test-pattern2 my-test-pattern2 'foo)

   (same-result test-pattern3 my-test-pattern3 (cons 4 3))
   (same-result test-pattern3 my-test-pattern3 '())
   (same-result test-pattern3 my-test-pattern3 'foo)
   ))
