(in-test-group
 post-processing

 (define-each-check
   (lset= eq? '(a d) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))
   (not (tidy-non-soundness
	 ;; Carelessly inlining y will change the scope of x
	 '(let ((x (vector 1 2)))
	    (cons x (cons x (let ((y (vector 3 x))
				  (x (vector 4)))
			      (cons x (cons x (vector-ref (vector-ref y 1) 1)))))))))))
