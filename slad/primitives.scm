(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (unary-primitive name proc)
  (add-primitive! (make-slad-primitive name proc)))

(define (binary-primitive name proc)
  (add-primitive! (make-slad-primitive name (lambda (arg)
					      (proc (slad-car arg) (slad-cdr arg))))))

(define-syntax define-unary-primitive
  (syntax-rules ()
    ((_ name)
     (unary-primitive 'name name))))

(define-syntax define-binary-primitive
  (syntax-rules ()
    ((_ name)
     (binary-primitive 'name name))))

(define-binary-primitive +)
(define-binary-primitive -)
(define-binary-primitive *)
(define-binary-primitive /)

(define-binary-primitive <)
(define-binary-primitive <=)
(define-binary-primitive >)
(define-binary-primitive >=)
(define-binary-primitive =)

(define-unary-primitive null?)
(define-unary-primitive boolean?)
(define-unary-primitive pair?)

(define (slad-procedure? thing)
  (or (slad-primitive? thing)
      (slad-closure? thing)))

(unary-primitive 'procedure? slad-procedure?)

(define-unary-primitive sqrt)
(define-unary-primitive exp)
(define-unary-primitive log)
(define-unary-primitive sin)
(define-unary-primitive cos)
(define-binary-primitive atan)
(define-unary-primitive zero?)
(define-unary-primitive positive?)
(define-unary-primitive negative?)
(define-unary-primitive real?)

(define (slad-if-procedure arg)
  (let ((p (car arg))
	(c (cadr arg))
	(a (cddr arg)))
    (if p
	(slad-apply c '())
	(slad-apply a '()))))

(add-primitive! (make-slad-primitive 'if-procedure slad-if-procedure))

(define-unary-primitive zero)
(binary-primitive 'bundle transform-and-perturb)
(unary-primitive 'primal slad-primal)
(unary-primitive 'tangent slad-tangent)

(define-binary-primitive set-forward-transform!)
