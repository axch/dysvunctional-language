(declare (usual-integrations))

(define-structure
  (primitive
   safe-accessors
   (print-procedure
    (simple-unparser-method 'primitive
     (lambda (prim)
       (list (primitive-name prim))))))
  name
  implementation)

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (unary-primitive name proc)
  (add-primitive! (make-primitive name proc)))

(define (binary-primitive name proc)
  (add-primitive!
   (make-primitive name (lambda (arg)
			  (proc (car arg) (cdr arg))))))

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
  (or (primitive? thing)
      (closure? thing)))

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

(add-primitive! (make-primitive 'if-procedure slad-if-procedure))

(define (slad-write thing)
  (write thing)
  (newline)
  thing)

(define (slad-read arg)
  (read))

(unary-primitive 'write slad-write)
(unary-primitive 'write-real slad-write)
(unary-primitive 'read-real slad-read)

(define-unary-primitive zero)
(binary-primitive 'bundle transform-and-perturb)
(unary-primitive 'primal primal)
(unary-primitive 'tangent tangent)
(unary-primitive 'forward? bundle?)


(define unev-forward-transforms (make-eq-hash-table))

(define (has-unev-forward-transform? thing)
  (hash-table/lookup unev-forward-transforms thing
   (lambda (datum) #t)
   (lambda () #f)))

(define (unev-forward-transform thing)
  (hash-table/get unev-forward-transforms thing #f))

(define (set-forward-transform-unevaluated! thing transform)
  (hash-table/put! unev-forward-transforms thing transform))

(define (transforms-to-self! name)
  (set-forward-transform-unevaluated! name name))

(define (operates-on-primal! name)
  (set-forward-transform-unevaluated!
   name
   `(lambda (b)
      (let* ((p (primal b))
	     (answer (,name p)))
	(bundle answer (zero answer))))))

(define (operates-on-primals! name)
  (set-forward-transform-unevaluated!
   name
   `(lambda (b1 b2)
      (let* ((p1 (primal b1))
	     (p2 (primal b2))
	     (answer (,name p1 p2)))
	(bundle answer (zero answer))))))

(transforms-to-self! 'null?)
(transforms-to-self! 'boolean?)
(transforms-to-self! 'pair?)
(transforms-to-self! 'procedure?)

(operates-on-primal! 'zero?)
(operates-on-primal! 'positive?)
(operates-on-primal! 'negative?)
(operates-on-primal! 'real?)

(operates-on-primal! 'write)
(operates-on-primal! 'write-real)
(operates-on-primal! 'read-real)

(define (unary-derivative-expression! name expr)
  (set-forward-transform-unevaluated!
   name
   `(lambda (b)
      (let ((p (primal b))
	    (t (tangent b)))
	(bundle
	 (,name p)
	 (* ,expr t))))))

(define (binary-derivative-expression! name exprx expry)
  (set-forward-transform-unevaluated!
   name
   `(lambda (x y)
      (let ((px (primal x))
	    (tx (tangent x))
	    (py (primal y))
	    (ty (tangent y)))
	(bundle
	 (,name px py)
	 (+ (* ,exprx tx) (* ,expry ty)))))))

(unary-derivative-expression!  'exp  '(exp p))
(unary-derivative-expression!  'sin  '(cos p))
(unary-derivative-expression!  'cos  '(- 0 (sin p)))
(unary-derivative-expression!  'sqrt '(/ 1 (* 2 (sqrt p))))
(binary-derivative-expression! '+ 1 1)
(binary-derivative-expression! '- 1 -1)
(binary-derivative-expression! '* 'py 'px)
(binary-derivative-expression! '/ '(/ 1 py) '(/ (- 0 px) (* py py)))

(operates-on-primals! '<)
(operates-on-primals! '<=)
(operates-on-primals! '>)
(operates-on-primals! '>=)
(operates-on-primals! '=)

(set-forward-transform-unevaluated!
 'if-procedure
 '(lambda (b1 c a)
    (if (primal b1)
	(c)
	(a))))

(transforms-to-self! 'bundle)
(transforms-to-self! 'primal)
(transforms-to-self! 'tangent)
(transforms-to-self! 'zero)
(transforms-to-self! 'forward?)


(define (initial-user-env)
  (let ((answer
	 (make-env
	  (map (lambda (primitive)
		 (cons (primitive-name primitive) primitive))
	       *primitives*))))
    (for-each (lambda (name)
		(set-forward-transform!
		 (cdr (assq name (env-bindings answer)))
		 (slad-eval (macroexpand (unev-forward-transform name)) answer)))
	      (filter has-unev-forward-transform?
		      (map car (env-bindings answer))))
    answer))
