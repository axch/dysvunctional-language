(define (analyzed-answer program)
  (let ((candidate
	 (let ((full-prog (macroexpand program)))
	   (let loop ((bindings (analysis-bindings (analyze program))))
	     (cond ((null? bindings) #f)
		   ((equal? full-prog (binding-exp (car bindings)))
		    (car bindings))
		   (else (loop (cdr bindings))))))))
    (if (not candidate)
	(error "Analysis makes no binding for the original program"
	       program)
	(binding-value candidate))))

(define (determined-form-breakage value form)
  (cond ((not (equal? (macroexpand form) (macroexpand (macroexpand form))))
	 `(not (equal? ,(macroexpand form) ,(macroexpand (macroexpand form)))))
	((not (equal? value (vl-eval form)))
	 `(not (equal? ,value (interpreted ,(vl-eval form)))))
	((not (equal? value (analyzed-answer form)))
	 `(not (equal? ,value (analyzed ,(analyzed-answer form)))))
	((not (equal? `(begin ,value) (analyze-and-generate form)))
	 `(not (equal? ,value (compiled ,(analyze-and-generate form)))))
	(else #f)))

(define (%scheme-eval code)
  (eval code (nearest-repl/environment)))

(define (eval-through-scheme program)
  (let* ((interpreted-answer (vl-eval program))
	 (analysis (analyze program))
	 (compiled-program (generate program analysis))
	 (compiled-answer (%scheme-eval compiled-program))
	 (pretty-compiled-answer (%scheme-eval (prettify-compiler-output compiled-program)))
	 (direct-pretty-compiled-answer (%scheme-eval (compile-to-scheme program))))
    (if (and (equal? interpreted-answer compiled-answer)
	     (equal? interpreted-answer pretty-compiled-answer)
	     (equal? interpreted-answer direct-pretty-compiled-answer))
	compiled-answer
	(error "VL compiler disagreed with VL interpreter"
	       `((interpreted: ,interpreted-answer)
		 (compiled: ,compiled-answer)
		 (compiled-and-prettified ,pretty-compiled-answer)
		 (pretty-compiled ,direct-pretty-compiled-answer))))))

