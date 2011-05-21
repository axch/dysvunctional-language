;;;; File:  load.scm -- Loader for rule system

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../auto-compilation")

(load-relative-compiled "utils")
(load-relative-compiled "../eq-properties")
(load-relative-compiled "../ghelper")
(load-relative-compiled "matcher")
(load-relative-compiled "pattern-directed-invocation")
(load-relative-compiled "simplification")
