(in-test-group
 post-processing

 (define-each-check
   (lset= eq? '(a d) (feedback-vertex-set '((a b c d) (b a) (c d) (d c) (e a))))
   (not (tidy-non-soundness
	 ;; Carelessly inlining y will change the scope of x
	 '(let ((x (vector 1 2)))
	    (cons x (cons x (let ((y (vector 3 x))
				  (x (vector 4)))
			      (cons x (cons x (vector-ref (vector-ref y 1) 1)))))))))

   ;; TODO How can I get the inlining not to clone the termination condition like that?
   (alpha-rename?
    '(begin
       (define (operation-4 n)
         (* n
            (let ((n (- n 1)))
              (if (= n 1)
                  1
                  (operation-4 n)))))
       (if (= (real 5) 1)
           1
           (operation-4 (real 5))))
    (compile-to-scheme
     '(let ()
        (define (fact n)
          (if (= n 1)
              1
              (* n (fact (- n 1)))))
        (fact (real 5)))))))
