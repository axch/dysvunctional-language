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

(define (meticulous-solved-analyze kernel-program answer)
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

(define (meticulous-generate program analysis answer)
  (let ((raw-fol (generate program analysis)))
    (check (equal? answer (fol-eval raw-fol)))
    raw-fol))

(define (meticulous-solved-generate program analysis answer)
  (let ((raw-fol (strip-argument-types
                  (meticulous-generate program analysis answer))))
    (check (equal? `(begin ,answer) raw-fol))
    raw-fol))

(define ((fol-carefully stage) fol-code)
  (let ((done (stage fol-code)))
    (if (eq? stage structure-definitions->vectors)
        ;; TODO Argh!  The input to structure-definitions->vectors
        ;; does not type check.
        (check-program-types done)
        (check (equal-type? (check-program-types done)
                            (check-program-types fol-code))))
    (check (equal? done (stage done)))
    done))

(define (carefully stage-data)
  (define (check-annotations program)
    (if (present? 'unique-names program)
        (check (unique-names? program)))
    (if (present? 'type program)
        (let ((annotated-type (property-value 'type program)))
          (check (equal-type? annotated-type
                              (check-program-types program)))))
    (if (present? 'a-normal-form program)
        (check (approximate-anf? program)))
    (if (present? 'lets-lifted program)
        (check (equal? program (lift-lets program))))
    (if (present? 'fully-inlined program)
        (check (equal? program (inline program))))
    (if (and (present? 'aggregates-replaced program)
             (present? 'a-normal-form program))
        (check (equal? program (scalar-replace-aggregates program))))
    (if (and (present? 'no-common-subexpressions program)
             (present? 'a-normal-form program)
             (present? 'lets-lifted program))
        (check (equal? program (intraprocedural-cse program))))
    (if (present? 'no-intraprocedural-dead-variables program)
        (check (equal? program
                       (eliminate-intraprocedural-dead-code program))))
    (if (present? 'no-interprocedural-dead-variables program)
        (check (equal? program
                       (eliminate-interprocedural-dead-code program))))
    program)
  (lambda (exec)
    (lambda (program)
      (check-annotations (exec program)))))

(define (((fol-meticulously answer) stage) fol-code)
  (let ((done ((fol-carefully stage) fol-code)))
    (check (equal? answer (fol-eval done)))
    done))

(define ((meticulously answer) stage-data)
  (lambda (exec)
    (lambda (program)
      (let ((done (((carefully stage-data) exec) program)))
        (check (equal? answer (fol-eval done)))
        done))))

(define (determined-answer program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (meticulous-solved-analyze kernel answer))
         (raw-fol (meticulous-solved-generate kernel analysis answer)))
    answer))

(define (optimizer how raw-fol)
  (let* ((vectors-fol ((how structure-definitions->vectors) raw-fol))
         (inlined ((how inline) vectors-fol))
         (anf ((how approximate-anf) inlined))
         (scalars (scalar-replace-aggregates anf)) ; SRA is not idempotent
         (cse ((how intraprocedural-cse) scalars))
         (variables ((how eliminate-intraprocedural-dead-variables) cse))
         (more-variables ((how interprocedural-dead-code-elimination) variables))
         (opt-fol ((how reverse-anf) more-variables))
         (type (check-program-types opt-fol)))
    ;; SRA emits sane code
    (check-program-types scalars)
; TODO    (check (equal? answer (fol-eval scalars)))

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
    (if (symbol? type)
        (begin
          (check (approximate-anf? scalars))
          (check (approximate-anf? cse))
          (check (approximate-anf? variables))
          (check (approximate-anf? more-variables))))

    ;; Except for reconstruction of the structure that the outside
    ;; world expects, SRA is idempotent and subsequent ANF-preserving
    ;; stages do not introduce new work for it.
    (if (symbol? type)
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

(define (compile-carefully program)
  (let* ((kernel (careful-macroexpand program))
         (analysis (analyze kernel))
         (raw-fol (generate kernel analysis)))
    #;(optimizer fol-carefully raw-fol)
    ((do-stages fol-optimize carefully)
     (structure-definitions->vectors raw-fol))))

(define (compile-meticulously program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (meticulous-generate kernel analysis answer)))
    #;(optimizer (fol-meticulously answer) raw-fol)
    ((do-stages fol-optimize (meticulously answer))
     (structure-definitions->vectors raw-fol))))

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

(define union-free-answer (union-free-answerer compile-meticulously))
(define loose-union-free-answer (union-free-answerer compile-carefully))
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
