(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (unary-primitive name proc)
  (add-primitive! (make-slad-primitive name proc)))

(define (binary-primitive name proc)
  (add-primitive! (make-slad-primitive name (lambda (arg)
					      (proc (slad-car arg) (slad-cdr arg))))))

(binary-primitive '%+ +)
(binary-primitive '- -)
(binary-primitive '* *)
(binary-primitive '/ /)

(unary-primitive '%exp exp)
(unary-primitive '%sin sin)
(unary-primitive '%cos cos)

(unary-primitive 'zero zero)
(binary-primitive 'bundle transform-and-perturb)
(unary-primitive 'primal slad-primal)
(unary-primitive 'tangent slad-tangent)

(binary-primitive 'with-forward-transform with-forward-transform)
