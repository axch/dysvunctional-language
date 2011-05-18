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
 '("nomenclature"
   "syntax"
   "runtime"
   "alpha-renaming"
   "structs"
   "type-check"
   "feedback-vertex-set"
   "inline"
   "a-normal-form"
   "srfi-11"
   "cse"
   "dead-code"
   "sra"
   "optimize"))
