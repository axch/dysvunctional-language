;;; Invariants:
;;;   macroexpand is idempotent
;;;   interpret produces the same answer as analyze, generate, and eval
;;;   for solved programs, analyze binds the answer to the program
;;;     and generate emits `(begin ,answer).
;;;   each stage of FOL compilation does not affect the answer
;;;   each stage of FOL compilation is idempotent
;;;   any chain of FOL stages ending in TIDY is idempotent
;;;   for union-free programs, optimized FOL does not cons
;;;   the code generator emits syntactically correct and type correct FOL
;;;   FOL compilation preserves syntactic and type correctness stepwise

(define (analyzed-answer program)
  (let ((full-prog (macroexpand program)))
    (let loop ((bindings (analysis-bindings (analyze program))))
      (cond ((null? bindings)
             (error "Analysis makes no binding for the original program"
                    program))
            ((equal? full-prog (binding-exp (car bindings)))
             (binding-value (car bindings)))
            (else (loop (cdr bindings)))))))

(define (determined-form-breakage value form)
  (cond ((not (equal? (macroexpand form) (macroexpand (macroexpand form))))
	 `(not (equal? ,(macroexpand form) ,(macroexpand (macroexpand form)))))
	((not (equal? value (interpret form)))
	 `(not (equal? ,value (interpreted ,(interpret form)))))
	((not (equal? value (analyzed-answer form)))
	 `(not (equal? ,value (analyzed ,(analyzed-answer form)))))
	((not (equal? `(begin ,value) (analyze-and-generate form)))
	 `(not (equal? ,value (compiled ,(analyze-and-generate form)))))
	(else #f)))

(define (%scheme-eval code)
  (eval code (nearest-repl/environment)))

(define (eval-through-scheme program)
  (let* ((interpreted-answer (interpret program))
	 (analysis (analyze program))
	 (compiled-program (generate program analysis))
	 (compiled-answer (%scheme-eval compiled-program))
	 (pretty-compiled-answer (%scheme-eval (prettify-compiler-output compiled-program)))
	 (direct-pretty-compiled-answer (%scheme-eval (compile-to-scheme program))))
    (if (and (equal? interpreted-answer compiled-answer)
	     (equal? interpreted-answer pretty-compiled-answer)
	     (equal? interpreted-answer direct-pretty-compiled-answer))
	compiled-answer
	(error "Compiler disagreed with interpreter"
	       `((interpreted: ,interpreted-answer)
		 (compiled: ,compiled-answer)
		 (compiled-and-prettified ,pretty-compiled-answer)
		 (pretty-compiled ,direct-pretty-compiled-answer))))))

