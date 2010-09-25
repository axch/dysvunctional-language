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

(define (extend-env env formal-tree arg)
  (make-env
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
		   formal-tree arg))))
   env))

(define (empty-env? thing)
  (eq? thing #f))

(define flow-user-env #f)

(define (uncurry f)
  (lambda (lst)
    (f (car lst) (cadr lst))))

(define concrete-top-level
  `((+ . ,(uncurry +))
    (* . ,(uncurry *))))

(define (initialize-flow-user-env)
  (set! flow-user-env
	(make-env
	 (map (lambda (pair)
		(cons (car pair)
		      (make-primitive (cdr pair) #f)))
	      concrete-top-level)
	 #f)))
