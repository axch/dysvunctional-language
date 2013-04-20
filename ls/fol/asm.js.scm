(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-matching"))
;;;; Simplistic FOL to asm.js compiler

(define (fol->asm.js-syntax program)
  (define compile-definition
    (rule `(define ((? name) (?? formals))
             (argument-types (?? arg-types) (? return))
             (? body))
          `(function ,name ,formals
            ,@(map parameter-type-setter formals arg-types)
            ,@(local-variable-declarations body)
            ,@(compile-expression body return))))
  (let ((defns (program->definitions program)))
    `(module
      (stdlib foreign heap)
      ;; TODO Maybe declare heap views
      ,@(map compile-definition (filter procedure-definition? defns))
      %%main)))

