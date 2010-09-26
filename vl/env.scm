(define-structure (env (safe-accessors #t))
  bindings
  parent)

(define (lookup exp env)
  (if (constant? exp)
      exp
      (if (empty-env? env)
	  (error "Variable not found" exp)
	  (let scan ((bindings (env-bindings env)))
	    (if (null? bindings)
		(lookup exp (env-parent env))
		(if (eq? exp (caar bindings))
		    (cdar bindings)
		    (scan (cdr bindings))))))))

(define (append-bindings new-bindings old-bindings)
  (append new-bindings
	  (remove-from-bindings
	   (map car new-bindings)
	   old-bindings)))

(define (remove-from-bindings symbols bindings)
  (filter (lambda (binding)
	    (not (memq (car binding) symbols)))
	  bindings))

(define (flat-bindings env)
  (let loop ((env env))
    (if (empty-env? env)
	'()
	(append-bindings (env-bindings env) (loop (env-parent env))))))

(define (formal-bindings formal arg)
  (let walk ((name-tree (car formal))
	     (value-tree arg))
    (cond ((null? name-tree)
	   '())
	  ((symbol? name-tree)
	   (list (cons name-tree value-tree)))
	  ((and (pair? name-tree) (pair? value-tree))
	   (if (eq? (car name-tree) 'cons)
	       (append (walk (cadr name-tree) (car value-tree))
		       (walk (caddr name-tree) (cdr value-tree)))
	       (append (walk (car name-tree) (car value-tree))
		       (walk (cdr name-tree) (cdr value-tree)))))
	  (else
	   (error "Mismatched formal and actual parameter trees"
		  formal arg)))))

(define (extend-env env formal-tree arg)
  (make-env (formal-bindings formal-tree arg) env))

(define (empty-env? thing)
  (eq? thing #f))

(define vl-user-env #f)

(define (uncurry f)
  (lambda (lst)
    (f (car lst) (cadr lst))))

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (binary-numeric-primitive name base)
  (make-primitive
   name
   2
   (lambda (arg)
     (base (car arg) (cdr arg)))
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (let ((first-arg (car arg))
	       (second-arg (cdr arg)))
	   (if (or (abstract-real? first-arg)
		   (abstract-real? second-arg))
	       abstract-real
	       (base first-arg second-arg)))))))

(define (real x)
  (if (real? x)
      x
      (error "A non-real object is asserted to be real" x)))

(add-primitive! (binary-numeric-primitive '+ +))
(add-primitive! (binary-numeric-primitive '* *))
(add-primitive!
 (make-primitive
  'real
  1
  real
  (lambda (x)
    (cond ((abstract-all? x) abstract-all)
	  ((abstract-real? x) abstract-real)
	  ((number? x) abstract-real)
	  (else (error "Something known not to be a real number is declared real" x))))))

(define (initial-vl-user-env)
  (make-env
   (map (lambda (primitive)
	  (cons (primitive-name primitive) primitive))
	*primitives*)
   #f))

(define (initialize-vl-user-env)
  (set! vl-user-env (initial-vl-user-env)))
