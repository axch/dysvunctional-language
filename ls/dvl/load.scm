;;;; Loading the system

;;; Why are you reading this file?  You already know what it does.

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../support/auto-compilation")
(load-relative "../support/rule-system/load")
(load-relative "../fol/load")

(for-each
 load-relative-compiled
 '("../support/utils"
   "errors"
   "data"
   "../vl/env"
   "syntax"
   "macro"
   "letrec"
   "eval"
   "analysis"
   "abstract-values"
   "abstract-eval"
   "../vl/nomenclature"
   "../vl/code-generator"
   "primitives"
   "read"))

(define stdlib-file
  (string-append (->namestring (self-relatively working-directory-pathname))
                 "stdlib.dvl"))

(define (dvl-prepare form)
  (let* ((stdlib (read-source stdlib-file))
         (program `(let () ,@stdlib ,form)))
    program))

(define (dvl-supersedes? name)
  (memq name '(derivative derivative-f derivative-using-j* j*
               gradient-f jacobian-using-j* car cdr)))

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

(define (dvl-read-file filename)
  (dvl-prepare (vlad->dvl `(let () ,@(read-source filename)))))

(define dvl-source dvl-read-file)

(define (dvl-run-file filename)
  (let* ((program (dvl-read-file filename))
         (compiled-program (compile-to-fol program)))
    (let ((scm-file (->namestring (pathname-new-type filename #f))))
      (fol->floating-mit-scheme compiled-program scm-file)
      (fluid-let ((load/suppress-loading-message? #t))
        (run-mit-scheme scm-file)))))
