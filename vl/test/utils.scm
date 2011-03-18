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

(define (tidy-non-soundness program)
  (cond ((not (equal? (fol-eval program)
		      (fol-eval (tidy (full-alpha-rename program)))))
	 `(not (equal? ,(fol-eval program)
		       (after-tidy ,(fol-eval (tidy (full-alpha-rename program)))))))
	(else #f)))

(define (for-each-example filename proc)
  (with-input-from-file filename
    (lambda ()
      (let* ((first (read))
             (second (read)))
        (let loop ((first first)
                   (second second)
                   (third (read)))
          (cond ((eof-object? first)
                 'done)
                ((eq? '===> second)
                 (proc first third)
                 (let* ((new-first (read))
                        (new-second (read)))
                   (loop new-first new-second (read))))
                (else
                 (proc first)
                 (loop second third (read)))))))))

(define (define-union-free-example-test program #!optional value)
  (if (not (default-object? value))
        (define-test
          (check (equal? value (union-free-answer program))))
        (define-test
          ;; At least check that interpret and compile-to-scheme agree
          (union-free-answer program))))
