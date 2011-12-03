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

(define (dvl-run-file filename)
  (let* ((program (dvl-source filename))
         (compiled-program (compile-to-fol program)))
    (let ((scm-file (->namestring (pathname-new-type filename #f))))
      (fol->floating-mit-scheme compiled-program scm-file)
      (fluid-let ((load/suppress-loading-message? #t))
        (run-mit-scheme scm-file)))))

(define (dvl-watch-benchmark name program)
  (define commas number->string-with-commas)
  (format #t "Starting benchmark ~A\n" name)
  (define starting-memory (gc-flip))
  (format #t "~@11A words free\n" (commas starting-memory))

  (define true-program (dvl-program program))
  (define initial-memory (gc-flip))
  (define initial-prime-numbers-stream-size
    (estimate-space-used-by-prime-numbers-stream))

  (format #t "Analyzing ~A...\n" name)
  (define analysis-run-time)
  (define analysis-gc-time)
  (define analysis (with-timings (lambda () (analyze true-program))
                    (lambda (run gc real)
                      (set! analysis-run-time run)
                      (set! analysis-gc-time gc)
                      (format #t "Analyzing ~A took ~A RUN + ~A GC\n" name run gc))))

  (gc-flip)
  (hash-table/clean! abstract-hash-cache-table)
  (hash-table/clean! free-variables-cache)
  (define post-analysis-memory (gc-flip))
  (define analysis-used (- initial-memory post-analysis-memory))
  (format #t "~@11A words eaten by analyze\n" (commas analysis-used))
  (define post-analysis-prime-numbers-stream-size
    (estimate-space-used-by-prime-numbers-stream))
  (define canonical-cache-count (hash-table/count canonical-abstract-values))
  ;; (reset-canonical-abstract-values!)
  ;; (define post-analysis-clear-canonical-cache-memory (gc-flip))
  (when (< (* 5 analysis-used) initial-memory)
    (define analysis-object-used (estimate-space-usage analysis))
    (format #t "~@11A words used by the analysis object\n" (commas analysis-object-used))
    (format #t "~@11A words used elsewhere\n" (commas (- analysis-used analysis-object-used))))
  ;; (format #t "~@11A words freed by clearing the canonical abstract value cache (had ~A entries)\n"
  ;;         (commas (- post-analysis-clear-canonical-cache-memory post-analysis-memory))
  ;;         (commas canonical-cache-count))
  (format #t "~@11A words used by prime numbers stream (before measuring analysis)\n"
          (commas (- post-analysis-prime-numbers-stream-size initial-prime-numbers-stream-size)))

  (define abstract-hash-count (hash-table/count abstract-hash-cache-table))
  (define free-variables-count (hash-table/count free-variables-cache))
  (format #t "~@11A entries in canonical abstract value cache\n" (commas canonical-cache-count))
  (format #t "~@11A entries in abstract-hash cache\n" (commas abstract-hash-count))
  (format #t "~@11A entries in free-variables cache\n" (commas free-variables-count))

  (format #t "Generating ~A...\n" name)
  (define generation-run-time)
  (define generation-gc-time)
  (define raw-fol (with-timings (lambda () (generate true-program analysis))
                   (lambda (run gc real)
                     (set! generation-run-time run)
                     (set! generation-gc-time gc)
                     (format #t "Generating ~A took ~A RUN + ~A GC\n" name run gc))))
  (print-short-fol-stats raw-fol)

  (gc-flip)
  (hash-table/clean! abstract-hash-cache-table)
  (hash-table/clean! free-variables-cache)
  (define post-generation-memory (gc-flip))
  (define analyze-and-generate-used (- initial-memory post-generation-memory))
  (format #t "~@11A words eaten by analyze-and-generate\n" (commas analyze-and-generate-used))

  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table))
  (set! *escaper-names* (make-abstract-hash-table))
  (define post-name-cache-clear-memory (gc-flip))
  (format #t "~@11A words freed by clearing the name caches\n"
          (commas (- post-name-cache-clear-memory post-generation-memory)))

  (define canonical-cache-count-2 (hash-table/count canonical-abstract-values))
  (reset-canonical-abstract-values!)
  (define post-reset-canonical-memory (gc-flip))
  (format #t "~@11A words freed by reclearing the canonical abstract value cache (had ~A entries)\n"
          (commas (- post-reset-canonical-memory post-name-cache-clear-memory))
          (commas canonical-cache-count-2))

  (define abstract-hash-count-2 (hash-table/count abstract-hash-cache-table))
  (hash-table/clear! abstract-hash-cache-table)
  (define post-abstract-hash-clear-memory (gc-flip))
  (format #t "~@11A words freed by clearing the abstract-hash cache (had ~A entries)\n"
          (commas (- post-abstract-hash-clear-memory post-reset-canonical-memory))
          (commas abstract-hash-count-2))

  (define free-variables-count-2 (hash-table/count free-variables-cache))
  (hash-table/clear! free-variables-cache)
  (define post-free-variables-clear-memory (gc-flip))
  (format #t "~@11A words freed by clearing the free-variables cache (had ~A entries)\n"
          (commas (- post-free-variables-clear-memory post-abstract-hash-clear-memory))
          (commas free-variables-count-2))

  (set! analysis #f)
  (define post-clear-analysis-memory (gc-flip))
  (format #t "~@11A words freed by clearing the analysis\n"
          (commas (- post-clear-analysis-memory post-free-variables-clear-memory)))

  (define new-prime-numbers-stream-size
    (estimate-space-used-by-prime-numbers-stream))
  (define raw-fol-space-usage (estimate-space-usage raw-fol))
  (format #t "~@11A words used by raw fol\n" (commas raw-fol-space-usage))

  (format #t "~@11A words used by prime numbers stream (before measuing raw fol size)\n"
          (commas (- new-prime-numbers-stream-size initial-prime-numbers-stream-size)))

  (format #t "~@11A words not accounted for (interned symbols?)\n"
          (commas (- (- initial-memory post-clear-analysis-memory)
                     raw-fol-space-usage
                     (- new-prime-numbers-stream-size initial-prime-numbers-stream-size))))

  (format #t "Optimizing ~A...\n" name)
  (define optimization-run-time)
  (define optimization-gc-time)
  (define opt-fol (with-timings (lambda () (fol-optimize raw-fol))
                   (lambda (run gc real)
                     (set! optimization-run-time run)
                     (set! optimization-gc-time gc)
                     (format #t "Optimizing ~A took ~A RUN + ~A GC\n" name run gc))))
  (print-short-fol-stats opt-fol)
#;
  (format #t "~A | ~A RUN + ~A GC for ~A words | ~A RUN + ~A GC for ~A more words | ~A RUN + ~A GC for ~A final words\n"
          name analysis-run-time analysis-gc-time analysis-used
          generation-run-time generation-gc-time (- analyze-and-generate-used analysis-used)
          optimization-run-time optimization-gc-time (estimate-space-usage opt-fol))

  ;; (format #t "Compiling...\n")
  ;; (show-time (lambda () (fol->floating-mit-scheme opt-fol name)))
  ;; (newline)

  ;; (format #t "Running...\n")
  ;; (abegin1 (show-time (lambda () (run-mit-scheme name)))
  ;;   (newline))
)

(define dvl-benchmarks
  `(("sqrt"          ((include-definitions "examples/sqrt.dvl")
                      ((derivative nr-sqrt) 4)))
    ("celestial"     ((include "examples/celestial.dvl")))
    ("saddle-FF"     ((include-definitions "../../../stalingrad/examples/automatic/saddle.vlad")
                      (do-saddle gradient-F gradient-F)))
    ("saddle-FR"     ((include-definitions "../../../stalingrad/examples/automatic/saddle.vlad")
                      (do-saddle gradient-F gradient-R)))
    ("saddle-RF"     ((include-definitions "../../../stalingrad/examples/automatic/saddle.vlad")
                      (do-saddle gradient-R gradient-F)))
    ("saddle-RR"     ((include-definitions "../../../stalingrad/examples/automatic/saddle.vlad")
                      (do-saddle gradient-R gradient-R)))
    ("particle-FF"   ((include-definitions "../../../stalingrad/examples/automatic/particle.vlad")
                      (particle gradient-F gradient-F)))
    ("particle-FR"   ((include-definitions "../../../stalingrad/examples/automatic/particle.vlad")
                      (particle gradient-F gradient-R)))
    ("particle-RF"   ((include-definitions "../../../stalingrad/examples/automatic/particle.vlad")
                      (particle gradient-R gradient-F)))
    ("particle-RR"   ((include-definitions "../../../stalingrad/examples/automatic/particle.vlad")
                      (particle gradient-R gradient-R)))
    ("prob-lambda-F" ((include-definitions "../../../stalingrad/examples/automatic/probabilistic-lambda-calculus.vlad")
                      (do-example gradient-ascent-F)))
    ("prob-lambda-R" ((include-definitions "../../../stalingrad/examples/automatic/probabilistic-lambda-calculus.vlad")
                      (do-example gradient-ascent-R)))
    ("prob-prolog-F" ((include-definitions "../../../stalingrad/examples/automatic/probabilistic-prolog.vlad")
                      (do-example gradient-ascent-F)))
    ("prob-prolog-R" ((include-definitions "../../../stalingrad/examples/automatic/probabilistic-prolog.vlad")
                      (do-example gradient-ascent-R)))
    ("backprop-F"    ((include-definitions "examples/multilayer-perceptron.dvl")
                      ((do-it gradient-F) (real 10000))))
    ("backprop-R"    ((include-definitions "examples/multilayer-perceptron.dvl")
                      ((do-it gradient-R) (real 10000))))))

(define (dvl-benchmark name)
  (let ((benchmark (assoc name dvl-benchmarks)))
    (if benchmark
        (apply dvl-watch-benchmark benchmark)
        (error "No such benchmark" name))))
