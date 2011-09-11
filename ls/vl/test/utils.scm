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

(define (carefully stage-data)
  (define (check-annotations program)
    (if (present? 'structures-as-vectors program)
        (check (equal? program (structure-definitions->vectors program))))
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
      ;; TODO Also check counterfactual invariants:
      ;; - Inlining commutes with ANF up to removal of aliases.  Why
      ;;   aliases?  Because inlining saves ANF work by naming the
      ;;   expressions that are arguments to inlined procedures.
      ;; - Inlining preserves ANF
      ;; - Inlining commutes with SRA+ANF up to aliases.
      (check-annotations (exec program)))))

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

(define (compile-carefully program)
  (let* ((kernel (careful-macroexpand program))
         (analysis (analyze kernel))
         (raw-fol (generate kernel analysis)))
    ((do-stages fol-optimize carefully) raw-fol)))

(define (compile-meticulously program)
  (let* ((kernel (careful-macroexpand program))
         (answer (interpret kernel))
         (analysis (analyze kernel))
         (raw-fol (meticulous-generate kernel analysis answer)))
    ((do-stages fol-optimize (meticulously answer)) raw-fol)))

(define ((union-free-answerer compile) program
         #!optional wallpaper? no-gensyms?)
  (if (default-object? wallpaper?)
      (set! wallpaper? #f))
  (if (default-object? no-gensyms?)
      (set! no-gensyms? #f))
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

    (if no-gensyms?
        (check (not (occurs-in-tree? 'gensym opt-fol))))

    (fol-eval opt-fol)))

(define union-free-answer (union-free-answerer compile-meticulously))
(define loose-union-free-answer (union-free-answerer compile-carefully))

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
        (check (equal? value (union-free-answer program *compilation-results-wallp* #t))))
      (define-test
        ;; At least check that interpret and compile-to-fol agree
        (union-free-answer program *compilation-results-wallp*))))

(define (define-loose-union-free-example-test program #!optional value)
  (if (not (default-object? value))
      (define-test
        (check (equal? value (loose-union-free-answer program *compilation-results-wallp* #t))))
      (define-test
        ;; At least check the FOL invariants are obeyed
        (loose-union-free-answer program *compilation-results-wallp*))))
