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

   (equal? "function fol_program(stdlib, foreign, heap) {
  \"use asm\";
  var heap_view = new stdlib.Float32Array(heap);
  var acos = stdlib.Math.acos;
  var asin = stdlib.Math.asin;
  var atan = stdlib.Math.atan2;
  var cos = stdlib.Math.cos;
  var sin = stdlib.Math.sin;
  var tan = stdlib.Math.tan;
  var exp = stdlib.Math.exp;
  var log = stdlib.Math.log;
  var sqrt = stdlib.Math.sqrt;
  var expt = stdlib.Math.pow;
  var abs = stdlib.Math.abs;
  function fact(n) {
    n = (+n);
    if ((n==0)) {
      return (+1);
    } else {
      return (+(n*fact((n-1))));
    }
  }
  function __main__() {
    return (+fact(4.));
  }
  return __main__;
}
" (prepare-for-asm.js factorial))

   (equal? "function fol_program(stdlib, foreign, heap) {
  \"use asm\";
  var heap_view = new stdlib.Float32Array(heap);
  var acos = stdlib.Math.acos;
  var asin = stdlib.Math.asin;
  var atan = stdlib.Math.atan2;
  var cos = stdlib.Math.cos;
  var sin = stdlib.Math.sin;
  var tan = stdlib.Math.tan;
  var exp = stdlib.Math.exp;
  var log = stdlib.Math.log;
  var sqrt = stdlib.Math.sqrt;
  var expt = stdlib.Math.pow;
  var abs = stdlib.Math.abs;
  function iteration(count, c_real, c_imag, z_real, z_imag) {
    count = (+count);
    c_real = (+c_real);
    c_imag = (+c_imag);
    z_real = (+z_real);
    z_imag = (+z_imag);
    if ((count<=0)) {
      heap_view[0] = z_real;
      heap_view[1] = z_imag;
      return;
    } else {
      return iteration((count-1), c_real, c_imag,
                       (((z_real*z_real)-(z_imag*z_imag))+c_real),
                       (((z_real*z_imag)+(z_imag*z_real))+c_imag));
    }
  }
  function __main__() {
    iteration(100, .5, .7, 0, 0);
    ans_real = heap_view[0];
    ans_imag = heap_view[1];
    return ((sqrt(((ans_real*ans_real)+(ans_imag*ans_imag)))<2)|0);
  }
  return __main__;
}
" (prepare-for-asm.js
   '(begin
      (define (iteration count c-real c-imag
                         z-real z-imag)
        (argument-types real real real real real (values real real))
        (if (<= count 0)
            (values z-real z-imag)
            (iteration (- count 1)
                       c-real c-imag
                       (+ (- (* z-real z-real)
                             (* z-imag z-imag))
                          c-real)
                       (+ (+ (* z-real z-imag)
                             (* z-imag z-real))
                          c-imag))))
      (let-values
          (((ans-real ans-imag)
            (iteration 100 .5 .7 0 0)))
        (< (sqrt
            (+ (* ans-real ans-real)
               (* ans-imag ans-imag)))
           2))))))


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

