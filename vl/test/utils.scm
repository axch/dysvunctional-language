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
  (let ((raw-fol (generate program analysis)))
    (check (equal? answer (fol-eval raw-fol)))
    raw-fol))

(define (careful-solved-generate program analysis answer)
  (let ((raw-fol (strip-argument-types
                  (careful-generate program analysis answer))))
    (check (equal? `(begin ,answer) raw-fol))
    raw-fol))

(define ((fol-carefully stage) fol-code answer)
  (let ((done (stage fol-code)))
    (check-program-types done)
    (check (equal? answer (fol-eval done)))
    (check (equal? done (stage done)))
    done))

(define (determined-answer program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (careful-solved-analyze kernel answer))
         (raw-fol (careful-solved-generate kernel analysis answer)))
    answer))

(define (union-free-answer program #!optional wallpaper?)
  (if (default-object? wallpaper?)
      (set! wallpaper? #f))
  (if wallpaper?
      (begin
        (display "***NEW PROGRAM ***")
        (newline)
        (pp program)))
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (careful-generate kernel analysis answer))
         (vectors-fol ((fol-carefully structure-definitions->vectors)
                       raw-fol answer))
         (inlined ((fol-carefully inline) vectors-fol answer))
         (alpha ((fol-carefully alpha-rename) inlined answer))
         (anf ((fol-carefully approximate-anf) alpha answer))
         (scalars (scalar-replace-aggregates anf)) ; SRA is not idempotent
         (aliases ((fol-carefully intraprocedural-de-alias) scalars answer))
         (variables ((fol-carefully eliminate-intraprocedural-dead-variables)
                     aliases answer))
         (tidied ((fol-carefully tidy) variables answer)))
    ;; SRA emits sane code
    (check-program-types scalars)
    (check (equal? answer (fol-eval scalars)))

    ;; The state of being maximally inlined is preserved (until
    ;; possibly dead code elimination and tidying)
    (check (equal? alpha (inline alpha)))
    (check (equal? anf (inline anf)))
    (check (equal? scalars (inline scalars)))
    (check (equal? aliases (inline aliases)))

    ;; Alpha renaming is preserved
    (check (no-shadowing? anf))
    (check (no-shadowing? scalars))
    (check (no-shadowing? aliases))
    (check (no-shadowing? variables))
    (check (no-shadowing? tidied))

    ;; Except for reconstruction of the structure that the outside
    ;; world expects, SRA and subsequent stages (except tidying)
    ;; preserve ANF.
    (if (not (pair? answer))
        (begin
          (check (approximate-anf? scalars))
          (check (approximate-anf? aliases))
          (check (approximate-anf? variables))))

    ;; Except for reconstruction of the structure that the outside
    ;; world expects, SRA is idempotent and subsequent ANF-preserving
    ;; stages do not introduce new work for it.
    (if (not (pair? answer))
        (begin
          (check (equal? scalars (scalar-replace-aggregates scalars)))
          (check (equal? aliases (scalar-replace-aggregates aliases)))
          (check (equal? variables (scalar-replace-aggregates variables)))))

    (define anf-then-inline (alpha-rename (inline (approximate-anf vectors-fol))))

    ;; Inlining commutes with ANF up to removal of aliases.  Why
    ;; aliases?  Because inlining saves ANF work by naming the
    ;; expressions that are arguments to inlined procedures.
    (check (alpha-rename? (intraprocedural-de-alias anf)
                          (intraprocedural-de-alias anf-then-inline)))

    ;; Inlining also preserves ANF
    (check (approximate-anf? anf-then-inline))

    ;; Likewise, inlining commutes with the ANF-SRA pair.
    (check (alpha-rename?
            (intraprocedural-de-alias scalars)
            (intraprocedural-de-alias
             (alpha-rename
              (inline
               (scalar-replace-aggregates (approximate-anf vectors-fol)))))))

    ;; Dead variable elimination does not introduce new aliases.
    (check (equal? variables (intraprocedural-de-alias variables)))

    ;; In the union-free case, SRA is successful at removing all
    ;; internal consing.
    (check (= 0 (count-free-occurrences 'car tidied)))
    (check (= 0 (count-free-occurrences 'cdr tidied)))
    (check (= 0 (count-free-occurrences 'vector-ref tidied)))

    (if wallpaper? (pp tidied))
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
		      (fol-eval (tidy (alpha-rename program)))))
	 `(not (equal? ,(fol-eval program)
		       (after-tidy ,(fol-eval (tidy (alpha-rename program)))))))
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

(define *compilation-results-wallp* #f)

(define (define-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (union-free-answer program *compilation-results-wallp*))))
      (define-test
        ;; At least check that interpret and compile-to-scheme agree
        (union-free-answer program *compilation-results-wallp*))))
