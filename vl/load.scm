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
(load-relative "support/rule-system/load")

(for-each
 load-relative-compiled
 '("support/mit-profile"
   "data"
   "env"
   "syntax"
   "macro"
   "letrec"
   "eval"
   "analysis"
   "abstract-values"
   "abstract-eval"
   "nomenclature"
   "code-generator"
   "primitives"
   "output-syntax"
   "post-processing"
   "read"))

(define (vl-run-file filename)
  (let* ((forms (read-source filename))
	 (program `(let () ,@forms))
	 (analysis (analyze program))
	 (compiled-program (generate program analysis))
	 (compiled-answer
	  (eval compiled-program (nearest-repl/environment))))
    (pp compiled-answer)))
