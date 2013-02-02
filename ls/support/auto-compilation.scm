(declare (usual-integrations))

;;;; Auto-compilation

;;; A facility for automatically (re)compiling files at load time so
;;; as to avoid both the hassle of manual recompilations and the
;;; slowness of running interpreted code.  Takes care around macros
;;; from previously loaded files.

(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename #!optional environment)
  (self-relatively (lambda () (load filename environment))))

(define (compiled-code-type)
  ;; Trying to support the C backend
  (if (lexical-unbound?
       (nearest-repl/environment)
       'compiler:compiled-code-pathname-type)
      "com"
      (compiler:compiled-code-pathname-type)))

;; The environment argument is the one to take macro definitions from
;; for sf.
(define (cf-conditionally filename #!optional environment)
  (define (default-environment)
    (if (current-eval-unit #f)
        (current-load-environment)
        (nearest-repl/environment)))
  (if (default-object? environment)
      (set! environment (default-environment)))
  (fluid-let ((sf/default-syntax-table environment))
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

(define (load-compiled filename #!optional environment)
  (if (compiler-available?)
      (begin (cf-conditionally filename environment)
	     (load filename environment))
      (if (compilation-seems-necessary? filename)
	  (begin (warn "The compiler does not seem to be loaded")
		 (warn "Are you running Scheme with --compiler?")
		 (warn "Skipping compilation; loading source interpreted")
		 (load (pathname-default-type filename "scm") environment))
	  (load filename environment))))

(define (load-relative-compiled filename #!optional environment)
  (self-relatively (lambda () (load-compiled filename environment))))

