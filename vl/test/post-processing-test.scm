(in-test-group
 post-processing

 (define-each-check
   (lset= eq? '(c a) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))
   (not (tidy-non-soundness
	 ;; Carelessly inlining y will change the scope of x
	 '(let ((x (vector 1 2)))
	    (cons x (cons x (let ((y (vector 3 x))
				  (x (vector 4)))
			      (cons x (cons x (vector-ref (vector-ref y 1) 1)))))))))

   (alpha-rename?
    '(begin
       (define (operation-2 n)
         (if (= n 1)
             1
             (* n (operation-2 (- n 1)))))
       (operation-2 (real 5)))
    (compile-to-scheme
     '(let ()
        (define (fact n)
          (if (= n 1)
              1
              (* n (fact (- n 1)))))
        (fact (real 5)))))))
