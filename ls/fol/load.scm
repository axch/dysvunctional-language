(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename #!optional environment)
  (self-relatively (lambda () (load filename environment))))

(load-relative "../support/auto-compilation")

(define fol-environment (make-top-level-environment))

(environment-define fol-environment 'fol-environment fol-environment)

(load-relative "../support/rule-system/load" fol-environment)

(for-each
 (lambda (file)
   (load-relative-compiled file fol-environment))
 '("../support/utils"
   "nomenclature"
   "syntax"
   "srfi-11"
   "runtime"
   "primitives"
   "alpha-renaming"
   "structs"
   "type-check"
   "feedback-vertex-set"
   "inline"
   "a-normal-form"
   "sra"
   "cse"
   "dead-code"
   "optimize"
   "mit-scheme"))

(let ((client-environment (the-environment)))
  (for-each
   (lambda (export)
     (environment-define
      client-environment export (environment-lookup fol-environment export)))
   '(fol-eval
     fol->mit-scheme
     fol->floating-mit-scheme
     fol->standalone-mit-scheme
     run-mit-scheme

     check-program-types
     fol-optimize
     optimize-visibly
     alpha-rename
     unique-names?
     alpha-rename?
     inline
     approximate-anf
     approximate-anf?
     scalar-replace-aggregates
     intraprocedural-cse
     eliminate-intraprocedural-dead-variables
     interprocedural-dead-code-elimination
     tidy
     strip-argument-types
     structure-definitions->vectors

     ;; TODO The FOL runtime system is part of the implementation of
     ;; VL and DVL (the concrete implementations of some primitives)
     real read-real write-real make-gensym gensym-number gensym? gensym=

     ;; The FOL names subsystem is also used by VL and DVL; TODO document
     make-name name->symbol reset-fol-names!
     )))
