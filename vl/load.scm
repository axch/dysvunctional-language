(define (cf-conditionally filename)
  (fluid-let ((sf/default-syntax-table (nearest-repl/environment)))
    (sf-conditionally filename))
  (if (not (file-processed? filename "bin" "com"))
      (compile-bin-file filename)))

(define (load-compiled filename)
  (cf-conditionally filename)
  (load filename))

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(define (load-relative-compiled filename)
  (self-relatively (lambda () (load-compiled filename))))

(for-each
 load-relative
 '("data"
   "env"
   "eval"
   "macro"
   "run"
   "abstract"
   "abstract-data"
   "code-generator"))

(define peephole-optimize #f)

(let ((rule-system "../../rule-system/load-for-use.scm"))
 (self-relatively
  (lambda ()
    (if (file-exists? rule-system)
	(begin (load rule-system)
	       (load "peephole-optimizer"))
	(warn "Did not find the rule-simplification engine, peephole optimizer disabled")))))
