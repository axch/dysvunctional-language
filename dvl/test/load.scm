(load-relative "../../testing/load")
(load-relative "../../vl/test/utils")

(define slad->dvl
  (on-subexpressions
   (rule-list
    (list
     (rule `(? thing ,number?)
           `(real ,thing))
     (rule '+ 'g:+)
     (rule '* 'g:*)))))

(define (in-and-loop expression)
  `(let ((x (gensym)))
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
   (equal? #t (determined-answer '(let ((x (gensym))) (gensym= x x))))
   (equal? #f (determined-answer '(let ((x (gensym))) (gensym= x (gensym)))))

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
        ;; The analysis could solve this with more accurate modeling
        ;; of possible gensym values
        (gensym= (gensym) (if (< (real 2) (real 1)) x (gensym))))))

   (equal?
    '(if (= (real 10) 0)
         #t
         #f)
    (compile-to-scheme
     (in-and-loop '(gensym= (gensym) (gensym)))))

   (equal? #t
    (union-free-answer
     (in-and-loop '(gensym= x (if (> (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     (in-and-loop '(gensym= x (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     (in-and-loop '(gensym= (gensym) (if (< (real 2) (real 1)) x (gensym))))))

   (equal? #f
    (union-free-answer
     (in-and-loop '(gensym= (gensym) (if (> (real 2) (real 1)) x (gensym))))))
   )

 (for-each-example "../vl/examples.scm" define-union-free-example-test)
 (for-each-example "../vl/test/test-vl-programs.scm"
                   define-union-free-example-test)

 ;; TODO Make the tangent-of-function test acceptably fast
 #;
 (define-test (tangent-of-function)
   (check (equal? 1 (union-free-answer
                     (dvl-prepare
                      '(let ()
                         (define (adder n)
                           (lambda (x)
                             (g:+ x n)))
                         (((derivative adder) (real 3)) (real 4))))))))

 ;; TODO Make compiling the essential examples acceptably fast
 #;
 (for-each-example "../slad/essential-examples.scm"
  (lambda (program #!optional value)
    (define-union-free-example-test
      (dvl-prepare (slad->dvl program)) value)))
 )
