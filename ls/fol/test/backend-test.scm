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
      (fact 4.0)))

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

   (equal? '((defstruct (point (:constructor make-point (x y)))
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
           (prepare-for-common-lisp magnitude))

   (equal? '((define (fact n)
               (if (= n 0.)
                   1.
                   (* n (fact (- n 1.)))))
             (write (fact 4.)))
           (prepare-for-stalin factorial))

   (equal? '((define (magnitude v)
               (sqrt
                (+ (* (vector-ref v 0) (vector-ref v 0))
                   (* (vector-ref v 1) (vector-ref v 1)))))
             (write (magnitude (vector 1. 1.))))
           (prepare-for-stalin magnitude))

   (equal? "function fol-program(stdlib, foreign, heap) {
  \"use asm\";
  var heap-view = stdlib.type(heap);
  function fact(n) {
    n = (+n);
    if ((n==0)) {
      return (+1);
    } else {
      return (+(n*fact((n-1))));
    }
  }
  function %%main() {
    return (+fact(4.));
  }
  return %%main;
}
" (prepare-for-asm.js factorial)))

 (for-each
  (lambda (program)
    (for-each
     (lambda (method)
       (define-each-check
         (equal? (fol-eval program) (method program))))
     (map (lambda (compile run)
            (lambda (program)
              (with-output-to-string
                (lambda ()
                  (compile program "test-compiler-output")))
              (run "test-compiler-output")))
          (list fol->mit-scheme
                fol->common-lisp)
          (list run-mit-scheme
                (lambda (base)
                  (with-input-from-string
                      (with-output-to-string (lambda () (run-common-lisp base)))
                    read))))))
  (list factorial magnitude))
 )
