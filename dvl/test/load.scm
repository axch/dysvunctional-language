(load-relative "../../testing/load")
(load-relative "../../vl/test/utils")

(in-test-group
 dvl
 (define-each-check
   (equal? 3 (determined-answer '(+ 1 2)))
   (equal? #f (determined-answer '(gensym= (gensym) (gensym))))
   (equal? #t (determined-answer '(let ((x (gensym))) (gensym= x x))))
   (equal? #f (determined-answer '(let ((x (gensym))) (gensym= x (gensym)))))

   (equal? #t (eval-through-scheme
	       '(let ((x (gensym))) (gensym= x (if (> (real 2) (real 1)) x (gensym))))))
   (equal? #f (eval-through-scheme
	       '(let ((x (gensym))) (gensym= x (if (< (real 2) (real 1)) x (gensym))))))
   )

 (with-input-from-file "../vl/examples.scm"
   (lambda ()
     (let loop ((program (read)))
       (if (not (eof-object? program))
	   (begin (define-test
		    ;; Check that interpret and compile-to-scheme agree
		    (eval-through-scheme program))
		  (loop (read)))))))

 (with-input-from-file "../vl/test/test-vl-programs.scm"
   (lambda ()
     (let loop ((program (read)))
       (if (not (eof-object? program))
	   (begin (define-test
		    ;; Check that interpret and compile-to-scheme agree
		    (eval-through-scheme program))
		  (loop (read)))))))

 ;; TODO Make the tangent-of-function test acceptably fast
 #;
 (define-test (tangent-of-function)
   (check (equal? 1 (eval-through-scheme
                     (dvl-prepare
                      '(let ()
                         (define (adder n)
                           (lambda (x)
                             (g:+ x n)))
                         (((derivative adder) (real 3)) (real 4))))))))
 )
