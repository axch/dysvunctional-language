;; The commandline script lands here

;; optimize n times; compile [via backend] are independent; execute
;; implies compile, but admits "interpreted fol" backend, for which
;; compiling is a no-op.

;; the mit scheme backend has a standalone option; Stalin, SBCL, and
;; Haskell are all standalone.

;; How to link to the compiled output afterwards?

;; fol [verb] file [via backend] [optimizing never|once|twice|thrice|n times|until convergence] [dumping <big-stage>] [adverb]*
(define (fol-main arg)
  (define (find-verb args)
    (cond ((null? args)
           (error "Need non-empty argument list"))
          ((memq (car args) '(optimize compile run))
           (car args))
          (else #f)))
  (define (parse-modifiers args)
    '())
  (let* ((args (with-input-from-string (string-append "(" arg ")") read))
         (verb (find-verb args))
         (args (if verb (cdr args) args))
         (file (symbol->string (car args)))
         (args (cdr args))
         (modifiers (parse-modifiers args)))
    (pp `(,verb ,file ,modifiers))
    (%exit 0)))
