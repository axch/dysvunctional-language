(declare (usual-integrations))
(in-test-group
 backend

 (define factorial
   '(begin
      (define (fact n)
        (argument-types real real)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))
      (fact 4)))

 (define magnitude
   '(begin
      (define-type point (structure (x real) (y real)))
      (define (magnitude v)
        (argument-types point real)
        (sqrt (+ (* (point-x v) (point-x v))
                 (* (point-y v) (point-y v)))))
      (magnitude (make-point 1 1))))

 (define-each-check
   (equal? '((labels
              ((fact (n)
                (declare (type double-float n))
                (declare (values double-float))
                (if (= n (coerce 0. 'double-float))
                    (coerce 1. 'double-float)
                    (* n (fact (- n (coerce 1. 'double-float))))))
               (__main__ () (fact (coerce 4. 'double-float))))
              (setf (fdefinition '__main__) (function __main__))))
           (prepare-for-common-lisp factorial))

   (equal? '((defstruct (point (constructor make-point (x y)))
               (x nil :type double-float :read-only t)
               (y nil :type double-float :read-only t))
             (labels
              ((magnitude (v)
                (declare (type point v))
                (declare (values double-float))
                (sqrt (+ (* (point-x v) (point-x v)) (* (point-y v) (point-y v)))))
               (__main__
                ()
                (magnitude
                 (make-point (coerce 1. 'double-float) (coerce 1. 'double-float)))))
              (setf (fdefinition '__main__) (function __main__))))
           (prepare-for-common-lisp magnitude)))
 )
