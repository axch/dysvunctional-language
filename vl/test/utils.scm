;;; Invariants:
;;;   macroexpand is idempotent
;;;   macroexpand produces syntactically correct kernel language
;;;   interpret produces the same answer as analyze-generate-eval
;;;   for solved programs, analyze binds the answer to the program
;;;     and generate emits `(begin ,answer).
;;;   each stage of FOL compilation does not affect the answer
;;;   each stage of FOL compilation is idempotent
;;;   any chain of FOL stages ending in TIDY is idempotent
;;;   for union-free programs, optimized FOL does not cons
;;;   the code generator emits syntactically correct and type correct FOL
;;;   FOL compilation preserves syntactic and type correctness stepwise

(define (careful-macroexpand program)
  (let ((answer (macroexpand program)))
    (check (equal? answer (macroexpand answer)))
    answer))

(define (careful-solved-analyze kernel-program answer)
  (let ((analysis (analyze kernel-program)))
    (let loop ((bindings (analysis-bindings analysis)))
      (cond ((null? bindings)
             (error "Analysis makes no binding for the original program"
                    program))
            ((equal? kernel-program (binding-exp (car bindings)))
             (check (equal? answer (binding-value (car bindings))))
             analysis)
            (else (loop (cdr bindings)))))))

(define (careful-generate program analysis answer)
  (let ((raw-fol (generate program analysis #t)))
    (check (equal? answer (fol-eval raw-fol)))
    raw-fol))

(define (careful-solved-generate program analysis answer)
  (let ((raw-fol (strip-argument-types
                  (careful-generate program analysis answer))))
    (check (equal? `(begin ,answer) raw-fol))
    raw-fol))

(define ((fol-carefully stage) fol-code answer)
  (let ((done (stage fol-code)))
    (check (equal? answer (fol-eval done)))
    (check (equal? done (stage done)))
    done))

(define (determined-answer program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (careful-solved-analyze kernel answer))
         (raw-fol (careful-solved-generate kernel analysis answer)))
    answer))

(define (union-free-answer program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (careful-generate kernel analysis answer))
         (vectors-fol ((fol-carefully structure-definitions->vectors)
                       raw-fol answer))
         (inlined ((fol-carefully inline) vectors-fol answer))
         (scalars ((fol-carefully scalar-replace-aggregates) inlined answer))
         (alpha ((fol-carefully full-alpha-rename) scalars answer))
         (tidied ((fol-carefully tidy) alpha answer)))
    (check (= 0 (count-free-occurrences 'car tidied)))
    (check (= 0 (count-free-occurrences 'cdr tidied)))
    (check (= 0 (count-free-occurrences 'vector-ref tidied)))
    answer))

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

(define (fol-eval code)
  (eval code (nearest-repl/environment)))

(define (eval-through-scheme program)
  (let* ((interpreted-answer (interpret program))
	 (analysis (analyze program))
	 (compiled-program (generate program analysis))
	 (compiled-answer (fol-eval compiled-program))
	 (pretty-compiled-answer (fol-eval (prettify-compiler-output compiled-program)))
	 (direct-pretty-compiled-answer (fol-eval (compile-to-scheme program))))
    (if (and (equal? interpreted-answer compiled-answer)
	     (equal? interpreted-answer pretty-compiled-answer)
	     (equal? interpreted-answer direct-pretty-compiled-answer))
	compiled-answer
	(error "Compiler disagreed with interpreter"
	       `((interpreted: ,interpreted-answer)
		 (compiled: ,compiled-answer)
		 (compiled-and-prettified ,pretty-compiled-answer)
		 (pretty-compiled ,direct-pretty-compiled-answer))))))

(define (tidy-non-soundness program)
  (cond ((not (equal? (fol-eval program)
		      (fol-eval (tidy (full-alpha-rename program)))))
	 `(not (equal? ,(fol-eval program)
		       (after-tidy ,(fol-eval (tidy (full-alpha-rename program)))))))
	(else #f)))
