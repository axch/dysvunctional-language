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

(load-relative "support/auto-compilation")

(for-each
 load-relative-compiled
 '("data"
   "env"
   "primitives"
   "macro"
   "eval"
   "analysis"
   "abstract-values"
   "abstract-eval"
   "nomenclature"
   "code-generator"
   "inline"))

(define post-processor #f)

(define (post-process code)
  (if post-processor
      (post-processor code)
      code))

(let ((rule-system "../../rule-system/load-for-use.scm"))
 (self-relatively
  (lambda ()
    (if (file-exists? rule-system)
	(begin (load rule-system)
	       (load-compiled "post-processor"))
	(begin (warn "Did not find the rule-simplification engine,")
	       (warn "post-processor disabled"))))))
