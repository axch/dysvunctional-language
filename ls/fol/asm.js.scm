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
      fol-program
      ;; View the heap as a collection of 32-bit floats
      (view-heap heap-view "Float32Array")
      ,@(map compile-definition (filter procedure-definition? defns))
      (return %%main))))

(define (js-heap-read name index)
  `(assign ,name ,(access heap-view ,index))) ; TODO Understand shifting

(define (js-heap-write expr index)
  `(assign (access heap-view ,index) ; TODO Understand shifting
           ,expr))

(define (js-coerce exp type)
  `(unary + ,exp)) ; Always "real" for now

(define (js-parameter-type-setter name type)
  `(assign ,name (unary + ,name))) ; Always "real" for now

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

(define (asm.js-syntax->printable syntax)
  (define-algebraic-matcher module-form (tagged-list? 'module) cadr caddr cdddr)
  (define-algebraic-matcher view-heap-form (tagged-list? 'view-heap) cadr caddr)
  (define-algebraic-matcher function-def (tagged-list? 'function) cadr caddr cdddr)
  (define-algebraic-matcher assign-form (tagged-list? 'assign) cadr caddr)
  (define-algebraic-matcher if-stmt-form (tagged-list? 'if-stmt) cadr caddr cadddr)
  (define-algebraic-matcher apply-form (tagged-list? 'apply) cadr caddr)
  (define-algebraic-matcher return-form (tagged-list? 'return) cadr)
  (define-algebraic-matcher unary-form (tagged-list? 'unary) cadr caddr)
  (define (intersperse items sublist)
    (if (null? items)
        '()
        (cons (car items)
              (append-map (lambda (item)
                            (list sublist item))
                          (cdr items)))))
  (let loop ((code syntax))
    (case* code
      ((module-form name args body)
       `("function " ,name "(stdlib, foreign, heap) {" nl
         (indent
          "\"use asm\";" nl
          ,@(map loop body))
         "}" nl))
      ((view-heap-form name type)
       `("var " ,name " = stdlib." type "(heap);" nl))
      ((function-def name args body)
       `("function " ,name "("
         ,@(intersperse args '("," breakable-space)) ") {" nl
         (indent
          ,@(map loop body)
          "}" nl)))
      ((assign-form var exp)
       `(,var " = " ,(loop exp) ";" nl))
      ((if-stmt-form pred cons alt)
       `(if " (" ,(loop pred) ") {" nl
         (indent
          ,@(map loop cons))
         "} else {" nl
         (indent
          ,@(map loop alt))
         "}" nl))
      ((apply-form func args)
       `(,func "(" ,@(intersperse (map loop args) '("," breakable-space)) ");" nl))
      ((return-form exp)
       `("return " ,(loop exp) ";" nl))
      ((unary-form op arg)
       `(,op ,(loop arg)))
      ((fol-var var) var)
      ((fol-const const) const))))
