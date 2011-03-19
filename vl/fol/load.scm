(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "../support/auto-compilation")
(load-relative "../support/rule-system/load")

(for-each
 load-relative-compiled
 '("syntax"
   "type-check"
   "feedback-vertex-set"
   "post-processing"
   "sra-anf"
   "srfi-11"
   "de-alias"
   "dead-code"
   "sra"))
