(in-test-group
 ad

 (define-each-check
   (= 5 (ad-eval 5 (make-ad-user-environment)))))
