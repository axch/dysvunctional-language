(declare (usual-integrations))
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
            ((and (eval-binding? (car bindings))
                  (equal? kernel-program (binding-exp (car bindings))))
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
    (check (equal? `(begin (let ((capture-1 ,answer)) capture-1)) raw-fol))
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

(define (compile-carefully program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (careful-generate kernel analysis answer))
         (vectors-fol ((fol-carefully structure-definitions->vectors)
                       raw-fol answer))
         (inlined ((fol-carefully inline) vectors-fol answer))
         (anf ((fol-carefully approximate-anf) inlined answer))
         (scalars (scalar-replace-aggregates anf)) ; SRA is not idempotent
         (cse ((fol-carefully intraprocedural-cse) scalars answer))
         (variables ((fol-carefully eliminate-intraprocedural-dead-variables)
                     cse answer))
         (more-variables ((fol-carefully interprocedural-dead-code-elimination)
                          variables answer))
         (opt-fol ((fol-carefully reverse-anf) more-variables answer)))
    ;; SRA emits sane code
    (check-program-types scalars)
    (check (equal? answer (fol-eval scalars)))

    ;; INLINE alpha renames; alpha renaming is preserved thereafter
    (check (unique-names? inlined))
    (check (unique-names? anf))
    (check (unique-names? scalars))
    (check (unique-names? cse))
    (check (unique-names? variables))
    (check (unique-names? more-variables))
    (check (unique-names? opt-fol))

    ;; The state of being maximally inlined is preserved (until
    ;; possibly dead code elimination)
    (check (equal? anf (inline anf)))
    (check (equal? scalars (inline scalars)))
    (check (equal? cse (inline cse)))

    ;; Except for reconstruction of the structure that the outside
    ;; world expects, SRA and subsequent stages (except REVERSE-ANF)
    ;; preserve ANF.
    (if (not (pair? answer))
        (begin
          (check (approximate-anf? scalars))
          (check (approximate-anf? cse))
          (check (approximate-anf? variables))
          (check (approximate-anf? more-variables))))

    ;; Except for reconstruction of the structure that the outside
    ;; world expects, SRA is idempotent and subsequent ANF-preserving
    ;; stages do not introduce new work for it.
    (if (not (pair? answer))
        (begin
          (check (equal? scalars (scalar-replace-aggregates scalars)))
          (check (equal? cse (scalar-replace-aggregates cse)))
          (check (equal? variables (scalar-replace-aggregates variables)))
          (check (equal? more-variables
                         (scalar-replace-aggregates more-variables)))))

    (define anf-then-inline (inline (approximate-anf (alpha-rename vectors-fol))))

    ;; Inlining commutes with ANF up to removal of aliases.  Why
    ;; aliases?  Because inlining saves ANF work by naming the
    ;; expressions that are arguments to inlined procedures.
    (check (alpha-rename? (intraprocedural-cse anf)
                          (intraprocedural-cse anf-then-inline)))

    ;; Inlining also preserves ANF
    (check (approximate-anf? anf-then-inline))

    ;; Likewise, inlining commutes with the ANF-SRA pair.
    (check (alpha-rename?
            (intraprocedural-cse scalars)
            (intraprocedural-cse
             (inline
              (scalar-replace-aggregates
               (approximate-anf (alpha-rename vectors-fol)))))))

    ;; Dead variable elimination does not introduce new common
    ;; subexpressions.
    (check (equal? variables (intraprocedural-cse variables)))
    (check (equal? more-variables (intraprocedural-cse more-variables)))

    opt-fol))

(define ((union-free-answerer compile) program #!optional wallpaper?)
  (if (default-object? wallpaper?)
      (set! wallpaper? #f))
  (if wallpaper?
      (begin
        (display "***NEW PROGRAM ***")
        (newline)
        (pp program)))
  (let ((opt-fol (compile program)))
    (if wallpaper? (pp opt-fol))
    ;; In the union-free case, SRA is successful at removing all
    ;; internal consing.
    (check (not (occurs-in-tree? 'car opt-fol)))
    (check (not (occurs-in-tree? 'cdr opt-fol)))
    (check (not (occurs-in-tree? 'vector-ref opt-fol)))

    (fol-eval opt-fol)))

(define union-free-answer (union-free-answerer compile-carefully))
(define fast-union-free-answer (union-free-answerer compile-to-scheme))

(define (analyzed-answer program)
  (let ((full-prog (macroexpand program)))
    (let loop ((bindings (analysis-bindings (analyze program))))
      (cond ((null? bindings)
             (error "Analysis makes no binding for the original program"
                    program))
            ((and (eval-binding? (car bindings))
                  (equal? full-prog (binding-exp (car bindings))))
             (binding-value (car bindings)))
            (else (loop (cdr bindings)))))))

(define (for-each-example filename proc)
  (with-input-from-file filename
    (lambda ()
      (let* ((first (read))
             (second (read)))
        (let loop ((first first)
                   (second second)
                   (third (read))
                   (definitions '()))
          (cond ((eof-object? first)
                 'done)
                ((definition? first)
                 (loop second third (read) (cons first definitions)))
                ((eq? '===> second)
                 (proc `(let ()
                          ,@(reverse definitions)
                          ,first)
                       third)
                 (let* ((new-first (read))
                        (new-second (read)))
                   (loop new-first new-second (read) definitions)))
                (else
                 (proc `(let ()
                          ,@(reverse definitions)
                          ,first))
                 (loop second third (read) definitions))))))))

(define *compilation-results-wallp* #f)

(define (define-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (union-free-answer program *compilation-results-wallp*))))
      (define-test
        ;; At least check that interpret and compile-to-scheme agree
        (union-free-answer program *compilation-results-wallp*))))

(define (define-fast-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (fast-union-free-answer program *compilation-results-wallp*))))
      (define-test
        ;; At least check that interpret and compile-to-scheme agree
        (fast-union-free-answer program *compilation-results-wallp*))))
