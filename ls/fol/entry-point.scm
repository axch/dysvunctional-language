(declare (usual-integrations))
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

(define backend-synopses
  '((mit-scheme . "Compile to native code via MIT Scheme")
    (floating-mit-scheme . "Like mit-scheme, but force floating-point arithmetic")
    (standalone-mit-scheme . "Like mit-scheme, but include FOL runtime")
    (stalin . "Compile to native code via the Stalin Scheme compiler")
    (common-lisp . "Generate Common Lisp and compile to native code via SBCL")))

(define-structure
  (task
   (print-procedure
    (simple-unparser-method 'task
                            (lambda (task)
                              (list (task-verb task)
                                    (task-backend task)
                                    (task-optimization task)
                                    (task-adverbs task))))))
  verb backend optimization adverbs)

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

(define (parse-task verb args)
  (define adverbs
    `((visibly . ,visibly)
      (volubly . ,volubly)
      (measuring-memory . ,measuring-memory)
      (watching-memory . ,watching-memory)
      (type-safely . ,type-safely)))
  (define the-task (make-task verb 'mit-scheme 1 '()))
  (define (backend choice)
    (if (memq choice (map car backend-compilers))
        (set-task-backend! the-task choice)
        (command-line-error "Unknown backend:" choice)))
  (define (opt-count choice)
    (define counts '((never . 0) (once . 1) (twice . 2) (thrice . 3)))
    (define level
      (cond ((assq choice counts)
             (cdr (assq choice counts)))
            ((integer? choice) choice)
            (else
             (command-line-error "Confusing optimization level:" choice))))
    (set-task-optimization! the-task level))
  (define (adverb stage)
    (set-task-adverbs! the-task (cons stage (task-adverbs the-task))))
  (let loop ((args args))
    (cond ((null? args) 'ok)
          ((eq? (car args) 'via)
           (if (null? (cdr args))
               (command-line-error "No backend specified with via."))
           (backend (cadr args))
           (loop (cddr args)))
          ((eq? (car args) 'optimizing)
           (if (null? (cdr args))
               (command-line-error "No level specified with optimizing."))
           (opt-count (cadr args))
           (loop (cddr args)))
          ((assq (car args) adverbs)
           (adverb (cdr (assq (car args) adverbs)))
           (loop (cdr args)))
          (else
           (command-line-error "Confusing option:" (car args)))))
  (set-task-adverbs! the-task (reverse (delete-duplicates (task-adverbs the-task))))
  the-task)

(define (command-line-error msg . irritants)
  (format-error-message msg irritants (current-output-port))
  (newline)
  (usage)
  (%exit 1))

(define (usage)
  (display "Usage: fol help [topic] OR fol [verb] file [adverb]...")
  (newline))

(define (help topics)
  (cond ((null? topics)
         (usage)
         (display "
The FOL subcommands are
  help      show this help, or describe backends or adverbs
  run       [default] optimize, compile, and execute FILE
  optimize  only optimize FILE, write result with .opt extension
  compile   optimize and compile FILE

The possible options are
  via <backend>  compile (and run) using the given backend.
                 See help backends for a list.
  optimizing n   Iterate optimization this many times.  In addition
                 to numbers, the words never, once, twice, and thrice
                 are accepted.
  <adverb>       Instrument optimization with the given adverb.
                 See help adverbs for a list.
"))
        ((eq? (car topics) 'backends)
         (synopsis-list "backends" backend-synopses))
        ((eq? (car topics) 'adverbs)
         (synopsis-list "adverbs"
          `((visibly . "With short progress reports")
            (volubly . "With longer statistics")
            (watching-memory . "With reports about memory use")
            (measuring-memory . "With reports about memory use and data size [expensive]")
            (type-safely . "Double-checking types of intermediate states"))))))

(define (synopsis-list topic synopses)
  (format #t "The supported ~A are\n" topic)
  (for-each
   (lambda (name-synopsis)
     (format #t "  ~21A  ~A\n" (car name-synopsis) (cdr name-synopsis)))
   synopses)
  (display "For more information, see the README or the source.")
  (newline))

;; fol [verb] file [via backend] [optimizing never|once|twice|thrice|n] [adverb]*
(define (fol-main arg)
  (define (find-verb args)
    (cond ((null? args)
           (help args)
           (%exit 1))
          ((memq (car args) '(optimize compile run))
           (if (null? (cdr args))
               (command-line-error "A file is required."))
           (car args))
          ((eq? (car args) 'help)
           (help (cdr args))
           (%exit 0))
          (else #f)))
  (let* ((args (with-input-from-string (string-append "(" arg ")") read))
         (verb (find-verb args))
         (args (if verb (cdr args) args))
         (verb (or verb 'run))
         (file (symbol->string (car args)))
         (args (cdr args))
         (task (parse-task verb args)))
    (let* ((program (with-input-from-file file read))
           (optimized-program (apply (optimization-step task) program (task-adverbs task))))
      (with-output-to-file (pathname-new-type file "opt")
        (lambda ()
          (pp program)
          (newline)))
      ((compilation-step task) program file)
      (pp ((execution-step task) file))
      (%exit 0))))
