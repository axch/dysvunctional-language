;; Abstract environments are just alists of bindings.  The
;; environments are expected to be flat, sorted, and precise, so that
;; they can be used effectively as keys in analysis bindings.
(define-structure (abstract-env (safe-accessors #t) (constructor %make-abstract-env))
  bindings)

(define (make-abstract-env bindings)
  (%make-abstract-env
   (sort
    bindings
    (lambda (binding1 binding2)
      (symbol<? (car binding1) (car binding2))))))

(define (abstract-lookup symbol env win lose)
  (let ((answer (assq symbol (abstract-env-bindings env))))
    (if answer
	(win (cdr answer))
	(lose))))

(define (env->abstract-env env)
  (make-abstract-env (env-bindings env)))

(define (env-slice env symbols)
  (define (restrict-bindings bindings)
    (filter (lambda (binding)
	      (memq (car binding) symbols))
	    bindings))
  (if (env? env)
      (make-env (restrict-bindings (env-bindings env)))
      (make-abstract-env (restrict-bindings (abstract-env-bindings env)))))

(define (extend-abstract-env formal arg abstract-env)
  (make-abstract-env
   (append-bindings (formal-bindings formal arg)
		    (abstract-env-bindings abstract-env))))

(define (interesting-variable? env)
  (lambda (var)
    (abstract-lookup var env
     (lambda (shape) (not (solved-abstractly? shape)))
     (lambda () #t))))

(define-structure (analysis (safe-accessors #t))
  bindings)

(define (analysis-lookup exp env analysis win lose)
  (let loop ((bindings (analysis-bindings analysis)))
    (if (null? bindings)
	(lose)
	(if (and (equal? exp (caar bindings))
		 (abstract-equal? env (cadar bindings)))
	    (win (caddar bindings))
	    (loop (cdr bindings))))))

(define (same-analysis-binding? binding1 binding2)
  (abstract-equal? binding1 binding2))

(define (same-analysis? ana1 ana2)
  (lset= same-analysis-binding? (analysis-bindings ana1)
	 (analysis-bindings ana2)))
