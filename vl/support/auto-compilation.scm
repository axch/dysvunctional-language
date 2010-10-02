;;; Only works on MIT Scheme
(define (compiled-code-type)
  ;; Trying to support the C backend
  (if (lexical-unbound?
       (nearest-repl/environment)
       'compiler:compiled-code-pathname-type)
      "com"
      (compiler:compiled-code-pathname-type)))

(define (cf-conditionally filename)
  (fluid-let ((sf/default-syntax-table (nearest-repl/environment)))
    (sf-conditionally filename))
  (if (cf-seems-necessary? filename)
      (compile-bin-file filename)))

(define (compiler-available?)
  (not (lexical-unbound? (nearest-repl/environment) 'cf)))

(define (compilation-seems-necessary? filename)
  (or (sf-seems-necessary? filename)
      (cf-seems-necessary? filename)))

(define (sf-seems-necessary? filename)
  (not (file-processed? filename "scm" "bin")))

(define (cf-seems-necessary? filename)
  (not (file-processed? filename "bin" (compiled-code-type))))

(define (load-compiled filename)
  (if (compiler-available?)
      (begin (cf-conditionally filename)
	     (load filename))
      (if (compilation-seems-necessary? filename)
	  (begin (warn "The compiler does not seem to be loaded")
		 (warn "Are you running Scheme with --compiler?")
		 (warn "Skipping compilation; loading source interpreted")
		 (load (pathname-default-type filename "scm")))
	  (load filename))))

(define (load-relative-compiled filename)
  (self-relatively (lambda () (load-compiled filename))))

