(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
        (with-working-directory-pathname
         (directory-namestring place)
         thunk)
        (thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../support/auto-compilation")

(for-each
 load-relative-compiled
 '("data"
   "read"
   "syntax"
   "macro"
   "letrec"
   "env"
   "eval"
   "forward-mode"
   "primitives"))