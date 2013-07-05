(declare (usual-integrations))
;;;; The commandline script lands here

;; optimize n times; compile [via backend] are independent; execute
;; implies compile, but admits "interpreted fol" backend, for which
;; compiling is a no-op.

;; the mit scheme backend has a standalone option; Stalin, SBCL, and
;; Haskell are all standalone.

;; How to link to the compiled output afterwards?

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
      (backend-compile (task-backend task))))

(define (execution-step task)
  (if (eq? (task-verb task) 'run)
      (backend-execute (task-backend task))
      (lambda args 'done)))

(define (parse-task verb args)
  (define adverbs
    `((visibly . ,visibly)
      (volubly . ,volubly)
      (measuring-memory . ,measuring-memory)
      (watching-memory . ,watching-memory)
      (type-safely . ,type-safely)))
  (define the-task (make-task verb (cdr (assq 'mit-scheme the-backends)) 1 '()))
  (define (backend choice)
    (if (assq choice the-backends)
        (set-task-backend! the-task (cdr (assq choice the-backends)))
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

(define (check-feasibility task)
  (let* ((backend (task-backend task))
         (verb (task-verb task))
         (problem (cond ((eq? verb 'compile)
                         ((backend-compiling-problem backend)))
                        ((eq? verb 'run)
                         (or ((backend-compiling-problem backend))
                             ((backend-executing-problem backend))))
                        (else #f))))
    (if problem
        (command-line-error
         "Cannot" verb (error-irritant/noise " via")
         (backend-name backend) (error-irritant/noise " because ")
         (error-irritant/noise problem) (error-irritant/noise ".")))))

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
         (synopsis-list "backends"
          (map cons
               (map car the-backends)
               (map backend-synopsis (map cdr the-backends)))))
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

;; A "verby command parser" parses command line strings of the form
;; command [verb] file [option]...
;; where the name of the command is assumed stripped from the string,
;; the verb is an optional item from a short list, and the file to
;; operate on is unique and required.
;;
;; The first verb in the argument list is the default if no verb is
;; supplied.  The win procedure is called with the specified verb and
;; the rest of the command after the file.  If the user wrote "help"
;; in the verb position, calls the help procedure to produce a help
;; message and exits.
(define (verby-command-parser help verbs win)
  (define (find-verb args)
    (cond ((null? args)
           (help args)
           (%exit 1))
          ((eq? (car args) 'help)
           (help (cdr args))
           (%exit 0))
          ((memq (car args) verbs)
           (if (null? (cdr args))
               (command-line-error "A file is required."))
           (car args))
          (else #f)))
  (lambda (arg)
    (let* ((args (with-input-from-string (string-append "(" arg ")") read))
           (verb (find-verb args))
           (args (if verb (cdr args) args))
           (verb (or verb (car verbs)))
           (file (symbol->string (car args)))
           (args (cdr args)))
      (values (win verb args) file))))

;; fol [verb] file [via backend] [optimizing never|once|twice|thrice|n] [adverb]*
(define (fol-main arg)
  (let-values (((task file) ((verby-command-parser help '(run optimize compile) parse-task) arg)))
    (check-feasibility task)
    (let* ((program (with-input-from-file file read))
           (optimized-program (apply (optimization-step task) program (task-adverbs task))))
      (with-output-to-file (pathname-new-type file "opt")
        (lambda ()
          (pp program)
          (newline)))
      ((compilation-step task) program file)
      ((execution-step task) file)
      (flush-output)
      (%exit 0))))
