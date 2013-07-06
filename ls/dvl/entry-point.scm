(declare (usual-integrations))


(define stdlib-file
  (string-append (->namestring (self-relatively working-directory-pathname))
                 "stdlib/stdlib.dvl"))

(define (dvl-prepare form)
  (let* ((stdlib (read-source stdlib-file))
         (program `(let () ,@stdlib ,form)))
    program))

(define (dvl-supersedes? name)
  (memq name '(derivative derivative-f derivative-using-j* j*
               gradient-f jacobian-using-j* car cdr
               gradient-r derivative-r derivative-using-*j j-transpose*v
               HACK-raise-to-same-bundle-level)))

(define (inexact-number? thing)
  (and (number? thing)
       (inexact? thing)))

(define (vlad->dvl forms)
  ((on-subexpressions
    (rule-list
     (list
      ;; TODO None of these rules should apply inside quoted data.  In
      ;; the case of the last one, the "right" thing (if we are
      ;; serious about treating all imprecise number literals as
      ;; unknown) would be to examine quoted data, see if it contains
      ;; any imprecise literals, and turn it into a quasiquote that
      ;; inserts `(real ,number).  Both of these things cannot be done
      ;; in the current setting because on-subexpressions applies the
      ;; rules bottom-up, so by the time any rule of the form '(quote
      ;; (? datum)) ... would see it, it's already too late.
      (rule '+ 'g:+)
      (rule '- 'g:-)
      (rule '* 'g:*)
      (rule '/ 'g:/)
      (rule 'sqrt 'g:sqrt)
      (rule 'exp 'g:exp)
      (rule 'log 'g:log)
      (rule 'sin 'g:sin)
      (rule 'cos 'g:cos)
      (rule 'atan 'g:atan)
      (rule '= 'g:=)
      (rule '< 'g:<)
      (rule '> 'g:>)
      (rule '<= 'g:<=)
      (rule '>= 'g:>=)
      (rule 'zero? 'g:zero?)
      (rule 'positive? 'g:positive?)
      (rule 'negative? 'g:negative?)
      (rule 'real? 'g:real?)
      (rule `(? number ,inexact-number?) `(real ,number)))))
   ((rule-simplifier
     (list
      (rule `((?? context1)
              (define ((? name ,dvl-supersedes?) (?? stuff)) (?? more-stuff))
              (?? context2))
            `(,@context1
              ,@context2)))) forms)))

(define (dvl-source filename)
  (dvl-prepare (vlad->dvl `(let () ,@(read-source filename)))))

(define (dvl-program forms #!optional basepath)
  (if (default-object? basepath)
      (set! basepath ".")) ;; TODO Or should this be the current file, namely the top of the DVL tree?
  (if (string? forms)
      (dvl-source forms)
      (dvl-prepare (vlad->dvl `(let () ,@(expand-toplevel-source forms basepath))))))

(define (dvl-file->fol-file filename)
  (let* ((program (dvl-source filename))
         (compiled-program (compile-to-fol program)))
    (let ((fol-file (->namestring (pathname-new-type filename "fol"))))
      (with-output-to-file fol-file
        (lambda ()
          (pp compiled-program)))
      compiled-program)))

(define (dvl-run-file filename)
  (let* ((compiled-program (dvl-file->fol-file filename)))
    (let ((scm-file (->namestring (pathname-new-type filename #f))))
      (fol->floating-mit-scheme compiled-program scm-file)
      (fluid-let ((load/suppress-loading-message? #t))
        (run-mit-scheme scm-file)))))

(define (dvl-main arg)
  (let ((verby-command-parser (access verby-command-parser fol-environment))
        (parse-task (access parse-task fol-environment))
        (check-feasibility (access check-feasibility fol-environment))
        (task-verb (access task-verb fol-environment))
        (task-adverbs (access task-adverbs fol-environment))
        (execute-task (access execute-task fol-environment))
        (fol-help (access help fol-environment)))
    (define (usage)
      (display "Usage: dvl help [topic] OR dvl [verb] file [adverb]...")
      (newline))
    (define (help topics)
      (if (null? topics)
          (begin
            (usage)
            (display "
The DVL subcommands are
  help      show this help, or describe backends or adverbs
  run       [default] analyze, optimize, compile, and execute FILE
  raw-fol   only analyze FILE, write unoptimized FOL with .fol extension
  optimize  analyze FILE to optimized FOL, write result with .opt extension
  compile   analyze, optimize and compile FILE

The possible options are
  via <backend>  compile (and run) using the given backend.
                 See help backends for a list.
  optimizing n   Iterate optimization this many times.  In addition
                 to numbers, the words never, once, twice, and thrice
                 are accepted.
  <adverb>       Instrument optimization with the given adverb.
                 See help adverbs for a list.
"))
          (fol-help topics)))
    (receive (task file) ((verby-command-parser help '(run optimize compile raw-fol) parse-task) arg)
      (check-feasibility task)
      (let* ((program (dvl-source file))
             (fol-program (apply analyze-and-generate program (task-adverbs task)))
             (fol-file (->namestring (pathname-new-type file "fol")))
             (verb (task-verb task)))
        ;; TODO Maybe avoid dumping the (large!) intermediate program if
        ;; the verb is not analyze?
        (with-output-to-file fol-file
          (lambda ()
            (pp fol-program)))
        (if (not (eq? 'analyze verb))
            (execute-task task fol-file))))))
