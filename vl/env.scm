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

(define (initial-vl-user-env)
  (make-env
   (map (lambda (primitive)
	  (cons (primitive-name primitive) primitive))
	*primitives*)
   #f))

(define (initialize-vl-user-env)
  (set! vl-user-env (initial-vl-user-env)))
