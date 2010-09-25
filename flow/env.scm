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

(define (formal-bindings formal-tree arg)
  (let walk ((name-tree formal-tree)
	     (value-tree arg))
    (cond ((null? name-tree)
	   '())
	  ((symbol? name-tree)
	   (list (cons name-tree value-tree)))
	  ((and (pair? name-tree) (pair? value-tree))
	   (append (walk (car name-tree) (car value-tree))
		   (walk (cdr name-tree) (cdr value-tree))))
	  (else
	   (error "Mismatched formal and actual parameter trees"
		  formal-tree arg)))))

(define (extend-env env formal-tree arg)
  (make-env (formal-bindings formal-tree arg) env))

(define (empty-env? thing)
  (eq? thing #f))

(define flow-user-env #f)

(define (uncurry f)
  (lambda (lst)
    (f (car lst) (cadr lst))))

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (binary-numeric-primitive name base)
  (make-primitive
   name
   (lambda (arg)
     (base (car arg) (cadr arg)))
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (let ((first-arg (car arg))
	       (second-arg (cadr arg)))
	   (if (or (abstract-real? first-arg)
		   (abstract-real? second-arg))
	       abstract-real
	       (base first-arg second-arg)))))))

(add-primitive! (binary-numeric-primitive '+ +))
(add-primitive! (binary-numeric-primitive '* *))

(define (initial-flow-user-env)
  (make-env
   (map (lambda (primitive)
	  (cons (primitive-name primitive) primitive))
	*primitives*)
   #f))

(define (initialize-flow-user-env)
  (set! flow-user-env (initial-flow-user-env)))
