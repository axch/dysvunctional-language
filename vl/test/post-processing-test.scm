(define (tidy-non-soundness program)
  (cond ((not (equal? (%scheme-eval program)
		      (%scheme-eval (tidy (full-alpha-rename program)))))
	 `(not (equal? ,(%scheme-eval program)
		       (after-tidy ,(%scheme-eval (tidy (full-alpha-rename program)))))))
	(else #f)))

(in-test-group
 post-processing

 (define-each-check
   (not (tidy-non-soundness
	 ;; Carelessly inlining y will change the scope of x
	 '(let ((x (vector 1 2)))
	    (cons x (cons x (let ((y (vector 3 x))
				  (x (vector 4)))
			      (cons x (cons x (vector-ref (vector-ref y 1) 1)))))))))))
