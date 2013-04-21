(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-matching"))
;;;; Simplistic FOL to asm.js compiler

(define (fol->asm.js-syntax program)
  (define (compile-statement exp #!optional type)
    (define (tail-position?)
      (not (default-object? type)))
    (define (compile-let-statement bindings body)
      `(,@(map compile-binding bindings)
        ,@(compile-statement body type)))
    (define (compile-binding b)
      (let ((name (car b))
            (exp (cadr b)))
        `(assign ,name ,(compile-expression exp))))
    (define (compile-let-values-statement names subexpr body)
      `(,@(compile-statement subexpr) ; Not tail position
        ,@(map js-heap-read ; Do I need to coerce types when reading from the heap?
               names
               (iota (length names)))
        ,@(compile-statement body type)))
    (define (compile-if-statement pred cons alt)
      `((if-stmt ,(compile-expression pred)
                 ,(compile-statement cons type)
                 ,(compile-statement alt type))))
    (define (compile-values-statement subforms)
      `(,@(map js-heap-write ; Do I need to coerce types when writing to the heap?
               (map compile-expression subforms)
               (iota (length subforms)))
        ,@(if (tail-position?)
              '((return))
              '())))
    (case* exp
      (let-form => compile-let-statement)
      (let-values-form => compile-let-values-statement)
      (if-form => compile-if-statement)
      (values-form => compile-values-statement)
      (_ (if (tail-position?)
             `((return ,(js-coerce (compile-expression exp) type)))
             `((statement ,(compile-expression exp)))))))
  (define (compile-expression exp)
    (case* exp
      ((fol-var var) var)
      ((fol-const const) const)
      ((lambda-form _ _) (error "Escaping procedures not supported for asm.js"))
      ((pair operator operands)
       `(apply ,operator ,(map compile-expression operands)))))
  (define (local-variable-declarations body)
    '()) ; TODO
  (define compile-definition
    (rule `(define ((? name) (?? formals))
             (argument-types (?? arg-types) (? return))
             (? body))
          `(function ,name ,formals
            ,@(map js-parameter-type-setter formals arg-types)
            ,@(local-variable-declarations body)
            ,@(compile-statement body return))))
  (let ((defns (program->definitions program)))
    `(module
      (stdlib foreign heap)
      ;; TODO Maybe declare heap views
      ,@(map compile-definition (filter procedure-definition? defns))
      %%main)))

(define (js-heap-read name index)
  `(assign ,name ,(access heap-view ,index))) ; TODO Understand shifting

(define (js-heap-write expr index)
  `(assign (access heap-view ,index) ; TODO Understand shifting
           ,expr))

(define (js-coerce exp type)
  `(+ ,exp)) ; Always "real" for now

(define (js-parameter-type-setter name type)
  `(assign ,name (+ ,name))) ; Always "real" for now

;;; The grammar that would be easy to translate to JS statements is
;;; block = <expression>
;;;       | (let ((<data-var> <expression>) ...) <block>)
;;;       | (let-values (((<data-var> <data-var> <data-var> ...) <expression>))
;;;           <block>)
;;;       | (if <expression> <block> <block>)
;;;       | (values <expression> <expression> <expression> ...)
;;;
;;; expression = <data-var> | <number> | <boolean> | ()
;;;            | (<proc-var> <expression> ...)
;;;
;;; access, construction, and lambda not actually allowed
;;;
;;; Unfortunately, normalizing FOL to this would require pulling ifs
;;; out of lets and if conditionals, which would lead to code
;;; duplication.  So I'll try to translate FOL entirely to JS
;;; expressions instead (except for 'return').
