;;;; File:  load.scm -- Loader for rule system

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "utils.scm")
(load-relative "eq-properties")
(load-relative "ghelper")
(load-relative "matcher")

(define (rule-memoize f) f)

(load-relative "pattern-directed-invocation")
(load-relative "rules")
