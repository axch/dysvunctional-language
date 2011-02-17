(declare (usual-integrations))

(define (slad-eval form env)
  (cond ((constant? form)
	 (constant-value form))
	((variable? form)
	 (lookup form env))
	((pair-form? form)
	 (make-slad-pair (slad-eval (car-subform form) env)
			 (slad-eval (cdr-subform form) env)))
	((lambda-form? form)
	 (make-slad-closure form env))
	((application? form)
	 (slad-apply (slad-eval (operator-subform form) env)
		     (slad-eval (operand-subform form) env)))
	(else
	 (error "Invalid expression type" form env))))

(define (slad-apply proc arg)
  (cond ((slad-closure? proc)
	 (slad-eval (slad-closure-body proc)
		    (extend-env (slad-closure-formal proc)
				arg
				(slad-closure-env proc))))
	((slad-primitive? proc)
	 ((slad-primitive-implementation proc) arg))
	(else
	 (error "Invalid procedure type" proc arg))))

(define my-pathname (self-relatively working-directory-pathname))
(define stdlib (string-append (->namestring my-pathname) "stdlib.slad"))

(define (slad-prepare form)
  `(let ()
     ,@(read-source stdlib)
     ,form))

(define (slad-do form)
  (slad-eval (macroexpand (slad-prepare form)) (initial-slad-user-env)))

(define (slad-eval-file filename)
  (let ((forms (read-source filename)))
    (write (slad-do `(let () ,@forms)))
    (newline)))
