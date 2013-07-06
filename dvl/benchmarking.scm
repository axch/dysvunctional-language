(declare (usual-integrations))

(define benchmarks-dir
  (string-append
   (->namestring (self-relatively working-directory-pathname))
   "benchmarks/"))

(define (dvl-watch-benchmark name program)
  (define basename (string-append benchmarks-dir name))
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

  (clear-name-caches!)
  (define post-name-cache-clear-memory (gc-flip))
  (format #t "~@11A words freed by clearing the name caches\n"
          (commas (- post-name-cache-clear-memory post-generation-memory)))

  (define canonical-cache-count-2 (hash-table/count canonical-abstract-values))
  (reset-canonical-abstract-values!)
  (define post-reset-canonical-memory (gc-flip))
  (format #t "~@11A words freed by clearing the canonical abstract value cache (had ~A entries)\n"
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
  (define opt-fol (with-timings (lambda () (loopy-fol-optimize raw-fol))
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

  (with-output-to-file
      (string-append basename ".fol")
    (lambda ()
      (pp opt-fol)))

  (format #t "Compiling ~A...\n" name)
  (define compilation-run-time)
  (define compilation-gc-time)
  (with-timings (lambda () (fol->floating-mit-scheme opt-fol basename))
   (lambda (run gc real)
     (set! compilation-run-time run)
     (set! compilation-gc-time gc)
     (format #t "Compiling ~A took ~A RUN + ~A GC\n" name run gc)))

  (format #t "Running ~A...\n" name)
  (define run-time)
  (define gc-time)
  (with-timings (lambda () (run-mit-scheme basename))
   (lambda (run gc real)
     (set! run-time run)
     (set! gc-time gc)
     (format #t "Running ~A took ~A RUN + ~A GC\n" name run gc)))

  (format #t "Compiling ~A with SBCL...\n" name)
  (define lisp-compilation-time)
  (with-timings (lambda () (fol->common-lisp opt-fol basename))
   (lambda (run gc real)
     (set! lisp-compilation-time real)
     (format #t "Compiling ~A took ~Ams\n" name real)))

  (format #t "Running ~A in SBCL...\n" name)
  (define lisp-run-time)
  (with-timings (lambda () (run-common-lisp basename))
   (lambda (run gc real)
     (set! lisp-run-time real)
     (format #t "Running ~A took ~Ams\n" name real)))
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
        ;; If not a predefined benchmark, assume is a filename that
        ;; should be executed.
        (dvl-watch-benchmark (->namestring (pathname-new-type name #f)) `((include ,name))))))
