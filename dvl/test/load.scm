(load-relative "../../testing/load")

(define (analyzed-answer program)
  (let ((candidate
	 (let ((full-prog (macroexpand program)))
	   (let loop ((bindings (analysis-bindings (analyze program))))
	     (cond ((null? bindings) '())
		   ((equal? full-prog (binding-exp (car bindings)))
		    (car bindings))
		   (loop (cdr bindings)))))))
    (if (not candidate)
	(error "Analysis makes no binding for the original program"
	       program)
	(binding-value candidate))))

(define (determined-form-breakage value form)
  (cond ((not (equal? (macroexpand form) (macroexpand (macroexpand form))))
	 `(not (equal? ,(macroexpand form) ,(macroexpand (macroexpand form)))))
	((not (equal? value (dvl-eval form #t)))
	 `(not (equal? ,value (interpreted ,(dvl-eval form)))))
	((not (equal? value (analyzed-answer form)))
	 `(not (equal? ,value (analyzed ,(analyzed-answer form)))))
	(else #f)))

(in-test-group
 dvl
 (define-each-check
   (not (determined-form-breakage 3 '(+ 1 2)))
   (not (determined-form-breakage #f '(gensym= (gensym) (gensym))))
   (not (determined-form-breakage #t '(let ((x (gensym))) (gensym= x x))))
   (not (determined-form-breakage #f '(let ((x (gensym))) (gensym= x (gensym)))))
   ))
