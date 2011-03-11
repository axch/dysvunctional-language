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

(load-relative "../vl/support/auto-compilation")
(load-relative "../vl/support/rule-system/load")

(for-each
 load-relative-compiled
 '("../vl/support/mit-profile"
   "data"
   "../vl/env"
   "syntax"
   "macro"
   "letrec"
   "eval"
   "analysis"
   "abstract-values"
   "abstract-eval"
   "work-list"
   "../vl/nomenclature"
   "../vl/code-generator"
   "primitives"
   "../vl/output-syntax"
   "../vl/post-processing"
   "read"))

(define stdlib-file
  (string-append (->namestring (self-relatively working-directory-pathname))
                 "stdlib.dvl"))

(define (dvl-prepare form)
  (let* ((stdlib (read-source stdlib-file))
         (program `(let () ,@stdlib ,form)))
    program))

(define (dvl-run-file filename)
  (let* ((forms (read-source filename))
         (program (dvl-prepare `(let () ,@forms)))
         (compiled-program (compile-to-scheme program))
         (compiled-answer
          (eval compiled-program (nearest-repl/environment))))
    (pp compiled-answer)))
