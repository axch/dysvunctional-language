;; The commandline script lands here

;; optimize n times; compile [via backend] are independent; execute
;; implies compile, but admits "interpreted fol" backend, for which
;; compiling is a no-op.

;; the mit scheme backend has a standalone option; Stalin, SBCL, and
;; Haskell are all standalone.

;; How to link to the compiled output afterwards?

(define backend-compilers
  `((mit-scheme . ,fol->mit-scheme)
    (floating-mit-scheme . ,fol->floating-mit-scheme)
    (standalone-mit-scheme . ,fol->standalone-mit-scheme)
    (stalin . ,fol->stalin)
    (common-lisp . ,fol->common-lisp)))

(define backend-runners
  `((mit-scheme . ,run-mit-scheme)
    (floating-mit-scheme . ,run-mit-scheme)
    (standalone-mit-scheme . #f)
    (stalin . #f)
    (common-lisp . ,run-common-lisp)))

(define-structure
  (task
   (print-procedure
    (simple-unparser-method 'task
                            (lambda (task)
                              (list (task-verb task)
                                    (task-backend task)
                                    (task-optimization task)
                                    (task-dumps task)
                                    (task-adverbs task))))))
  verb backend optimization dumps adverbs)

(define (optimization-step task)
  (repeat-stage fol-optimize (task-optimization task)))

(define (compilation-step task)
  (if (eq? (task-verb task) 'optimize)
      (lambda args 'ok)
      (cdr (assq (task-backend task) backend-compilers))))

(define (execution-step task)
  (if (eq? (task-verb task) 'run)
      (cdr (assq (task-backend task) backend-runners))
      (lambda args 'done)))

(define (dump-optimized? task)
  (or (eq? (task-verb task) 'optimize)
      (memq 'optimized (task-dumps task))))

(define (parse-task verb args)
  (define adverbs
    `((visibly . ,visibly)
      (volubly . ,volubly)
      (measuring-memory . ,measuring-memory)
      (watching-memory . ,watching-memory)
      (type-safely . ,type-safely)))
  (define the-task (make-task verb 'mit-scheme 1 '() '()))
  (define (backend choice)
    (if (memq choice (map car backend-compilers))
        (set-task-backend! the-task choice)
        (error "Unknown backend" choice)))
  (define (opt-count choice)
    (define counts '((never . 0) (once . 1) (twice . 2) (thrice . 3)))
    (set-task-optimization! the-task
                                 (if (assq choice counts)
                                     (cdr (assq choice counts))
                                     choice)))
  (define (dump stage)
    (if (memq stage '(optimized compiled))
        (set-task-dumps! the-task (cons stage (task-dumps the-task)))))
  (define (adverb stage)
    (set-task-adverbs! the-task (cons stage (task-adverbs the-task))))
  (let loop ((args args))
    (cond ((null? args) 'ok)
          ((eq? (car args) 'via)
           (backend (cadr args))
           (loop (cddr args)))
          ((eq? (car args) 'optimizing)
           (opt-count (cadr args))
           (loop (cddr args)))
          ((eq? (car args) 'dumping)
           (dump (cadr args))
           (loop (cddr args)))
          ((assq (car args) adverbs)
           (adverb (cdr (assq (car args) adverbs)))
           (loop (cdr args)))
          (else
           (error "Confusing option" (car args)))))
  (set-task-dumps! the-task (reverse (delete-duplicates (task-dumps the-task))))
  (set-task-adverbs! the-task (reverse (delete-duplicates (task-adverbs the-task))))
  the-task)

;; fol [verb] file [via backend] [optimizing never|once|twice|thrice|n] [dumping <big-stage>] [adverb]*
(define (fol-main arg)
  (define (find-verb args)
    (cond ((null? args)
           (error "Need non-empty argument list"))
          ((memq (car args) '(optimize compile run))
           (car args))
          (else #f)))
  (let* ((args (with-input-from-string (string-append "(" arg ")") read))
         (verb (find-verb args))
         (args (if verb (cdr args) args))
         (verb (or verb 'run))
         (file (symbol->string (car args)))
         (args (cdr args))
         (task (parse-task verb args)))
    (pp `(,verb ,file ,task))
    (let* ((program (with-input-from-file file read))
           (optimized-program (apply (optimization-step task) program (task-adverbs task))))
      (if (dump-optimized? task)
          (with-output-to-file (pathname-new-type file "opt")
            (lambda ()
              (pp program)
              (newline))))
      ((compilation-step task) program file)
      (pp ((execution-step task) file))
      (%exit 0))))
