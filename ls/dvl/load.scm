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

(load-relative "../support/auto-compilation")
(load-relative "../support/rules/load")
(load-relative "../fol/load")

(for-each
 load-relative-compiled
 '("../support/utils"
   "errors"
   "data"
   "../vl/env"
   "syntax"
   "macro"
   "letrec"
   "eval"
   "analysis"
   "abstract-values"
   "abstract-eval"
   "../vl/nomenclature"
   "../vl/code-generator"
   "primitives"
   "read"
   "entry-point"
   "benchmarking"))

;;; TODO Where does this procedure go?
(define (clear-analyze-and-generate-caches!)
  (clear-name-caches!)
  (reset-canonical-abstract-values!)
  (hash-table/clear! abstract-hash-cache-table)
  (hash-table/clear! free-variables-cache)
  (gc-flip)
  (hash-table/clean! (access eq-properties fol-environment)))
