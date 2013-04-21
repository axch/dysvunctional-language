(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-matching"))
;;;; Simplistic FOL to asm.js compiler

(define (prepare-for-asm.js program)
  (printable->string
   (asm.js-syntax->printable
    (fol->asm.js-syntax program))))

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
       (if (js-operator? operator)
           `(,(if (< (length operands) 2)
                  'unary
                  'binary)
             ,(js-operator operator)
             ,@(map compile-expression operands))
           `(apply ,operator ,(map compile-expression operands))))))
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
  `(assign ,name (access heap-view ,index))) ; TODO Understand shifting

(define (js-heap-write expr index)
  `(assign (access heap-view ,index) ; TODO Understand shifting
           ,expr))

(define (js-coerce exp type)
  (cond ((eq? type 'real)
         `(unary + ,exp))
        ((eq? type 'bool)
         ;; Booleans in asm.js are integers
         `(binary "|" ,exp 0))
        ((pair? type)
         ;; Multiple values are passed out of band
         exp)))

(define (js-parameter-type-setter name type)
  `(assign ,name (unary + ,name))) ; Always "real" for now

(define (js-operator? thing)
  (memq thing '(+ - * / = <= >= < >)))

(define (js-operator thing)
  (if (eq? thing '=)
      '==
      thing))

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
  (define (return-void? thing)
    (and (pair? thing)
         (null? (cdr thing))
         (eq? 'return (car thing))))
  (define-algebraic-matcher module-form (tagged-list? 'module) cadr cddr)
  (define-algebraic-matcher view-heap-form (tagged-list? 'view-heap) cadr caddr)
  (define-algebraic-matcher function-def (tagged-list? 'function) cadr caddr cdddr)
  (define-algebraic-matcher assign-form (tagged-list? 'assign) cadr caddr)
  (define-algebraic-matcher if-stmt-form (tagged-list? 'if-stmt) cadr caddr cadddr)
  (define-algebraic-matcher return-void-form return-void?)
  (define-algebraic-matcher return-form (tagged-list? 'return) cadr)
  (define-algebraic-matcher statement-form (tagged-list? 'statement) cadr)
  (define-algebraic-matcher apply-form (tagged-list? 'apply) cadr caddr)
  (define-algebraic-matcher unary-form (tagged-list? 'unary) cadr caddr)
  (define-algebraic-matcher binary-form (tagged-list? 'binary) cadr caddr cadddr)
  (define-algebraic-matcher heap-access-form (tagged-list? 'access) cadr caddr)
  (define (intersperse items sublist)
    (if (null? items)
        '()
        (cons (car items)
              (append-map (lambda (item)
                            (append sublist (list item)))
                          (cdr items)))))
  (let loop ((code syntax))
    (case* code
      ((module-form name body)
       `("function " ,name "(stdlib, foreign, heap) {" nl
         (indent
          "\"use asm\";" nl
          ,@(map loop body))
         "}" nl))
      ((view-heap-form name type)
       `("var " ,name " = stdlib." type "(heap);" nl))
      ((function-def name args body)
       `("function " ,name "("
         (align
          ,@(intersperse args '("," breakable-space))) ") {" nl
         (indent
          ,@(map loop body))
         "}" nl))
      ((assign-form var exp)
       `(,(loop var) " = " ,(loop exp) ";" nl))
      ((if-stmt-form pred cons alt)
       `(if " (" ,(loop pred) ") {" nl
         (indent
          ,@(map loop cons))
         "} else {" nl
         (indent
          ,@(map loop alt))
         "}" nl))
      ((return-void-form)
       `("return;" nl))
      ((return-form exp)
       `("return " ,(loop exp) ";" nl))
      ((statement-form exp)
       `(,(loop exp) ";" nl))
      ((apply-form func args)
       `(,func "(" (align ,@(intersperse (map loop args) '("," breakable-space))) ")"))
      ;; TODO Try to use precedence rules to minimize the number of
      ;; emitted parens?
      ((unary-form op arg)
       `("(" ,op ,(loop arg) ")"))
      ((binary-form op left right)
       `("(" ,(loop left) ,op ,(loop right) ")"))
      ((heap-access-form view index)
       `(,view "[" ,index "]")) ; TODO understand shifting?
      ((fol-var var) var)
      ((fol-const const) const))))

(define (printable->string instructions)
  (define width 80)
  (define (forced-width instructions)
    (cond ((null? instructions) 0)
          ((eq? (car instructions) 'nl) 0)
          ((eq? (car instructions) 'indent)
           (forced-width (cdr instructions)))
          ((eq? (car instructions) 'align)
           (forced-width (cdr instructions)))
          ((eq? (car instructions) 'breakable-space) 0)
          ((pair? (car instructions))
           (forced-width (append (car instructions) (cdr instructions))))
          (else
           (+ (string-length
               (with-output-to-string
                 (lambda ()
                   (display (car instructions)))))
              (forced-width (cdr instructions))))))
  (with-output-to-string
    (lambda ()
      (let loop ((indent-level 0)
                 (position 0)
                 (instructions instructions))
        (define (at-new-line?)
          (= position 0))
        (define (show string)
          (if (at-new-line?)
              (begin
                (for-each (lambda (i)
                            (display " "))
                          (iota indent-level))
                (display string)
                (+ (string-length string) indent-level))
              (begin
                (display string)
                (string-length string))))
        (cond ((null? instructions) position)
              ((eq? (car instructions) 'nl)
               (newline)
               (loop indent-level 0 (cdr instructions)))
              ((eq? (car instructions) 'indent)
               (loop (+ indent-level 2) position (cdr instructions)))
              ((eq? (car instructions) 'align)
               ;; TODO First indent if necessary?
               (loop position position (cdr instructions)))
              ((eq? (car instructions) 'breakable-space)
               (loop
                indent-level
                position
                (cons (if (> (+ position (forced-width (cdr instructions))) width)
                          'nl
                          " ")
                      (cdr instructions))))
              ((pair? (car instructions))
               (loop
                indent-level ; Unindent from subloops
                ;; but preserve the position
                (loop indent-level position (car instructions))
                (cdr instructions)))
              (else ; displayable object
               (let ((string (with-output-to-string
                               (lambda ()
                                 (display (car instructions))))))
                 (loop
                  indent-level
                  (+ position (show string))
                  (cdr instructions)))))))))
