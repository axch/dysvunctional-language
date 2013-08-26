;;; ----------------------------------------------------------------------
;;; Copyright 2013 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-case/pattern-case"))
;;;; Simplistic FOL to asm.js compiler

(define (fol->asm.js program #!optional base)
  (if (default-object? base)
      (set! base "jsozzle"))
  (let ((code (prepare-for-asm.js program))
        (file (pathname-new-type base "js")))
    (with-output-to-file file
      (lambda ()
        (display code)))))

(define (prepare-for-asm.js program)
  (printable->string
   (asm.js-syntax->printable
    (tweak-for-mandelbrot-example
     (fol->asm.js-syntax
      ;; The "real" primitive does nothing at runtime, and Firefox 23.0
      ;; mysteriously does not compile calls to it in the Javascript.
      ((on-subexpressions (rule '(real (? x)) x))
       program))))))

;;; I apologize for this unfortunate procedure.  As of this writing,
;;; the only function values that either DVL for FOL can export to the
;;; outside world must accept a single real number as an argument.
;;; Therefore, the only way to export a function of two real arguments
;;; (such as `mandelbrot?`, which needs the real and imaginary
;;; component of the point it is to test) is to curry it.
;;; Unfortunately, asm.js does not support higher-order procedures, so
;;; there is no direct translation for such a currying.  Instead, the
;;; mandel.dvl example opts for compiling an expression which
;;; represents a typical use of the `mandelbrot?` function.  This
;;; turns into a recognizable `__main__` procedure when converted to
;;; asm syntax, which I can tweak mid-flight to produce the desired
;;; effect.  This blemish will be removed when the DVL and FOL foreign
;;; interfaces are up to it, but in the meantime it seems better than
;;; asking people to edit the compiler's output by hand.
(define tweak-for-mandelbrot-example
  (rule '(module
           (?? stuff)
           (function __main__ ()
             (var (? v1) 0.)
             (var (? v2) 0.)
             (statement (apply (? op1) ((? ct) .5 .7 0. 0.)))
             (assign (? v1) (unary + (access heap_view 0)))
             (assign (? v2) (unary + (access heap_view 1)))
             (? return))
           (? export))
        `(module
           ,@stuff
           (function __main__ ("x" "y")
             (assign "x" (unary + "x"))
             (assign "y" (unary + "y"))
             (var ,v1 0.)
             (var ,v2 0.)
             (statement (apply ,op1 (,ct "x" "y" 0. 0.)))
             (assign ,v1 (unary + (access heap_view 0)))
             (assign ,v2 (unary + (access heap_view 1)))
             ,return)
           ,export)))

(define (fol->asm.js-syntax program)
  ;; TODO This inferred-type-map code is copied from common-lisp.scm.
  ;; Abstract.
  (define inferred-type-map (make-eq-hash-table))
  (for-each-fol-expression program
   (lambda (expr type)
     (hash-table/put! inferred-type-map expr type)))
  (define (lookup-inferred-type expr)
    (or (hash-table/get inferred-type-map expr #f)
        (error "Looking up unknown expression" expr)))
  (define (compile-statement exp tail-position?)
    (define (compile-let-statement bindings body)
      `(,@(map compile-binding bindings)
        ,@(compile-statement body tail-position?)))
    (define (compile-binding b)
      (let ((name (car b))
            (exp (cadr b)))
        `(assign ,(js-identifier name) ,(compile-expression exp))))
    (define (compile-let-values-statement names subexpr body)
      `(,@(compile-statement subexpr #f) ; Not tail position
        ,@(map js-heap-read
               (map js-identifier names)
               (iota (length names)))
        ,@(compile-statement body tail-position?)))
    (define (compile-if-statement pred cons alt)
      `((if-stmt ,(compile-expression pred)
                 ,(compile-statement cons tail-position?)
                 ,(compile-statement alt tail-position?))))
    (define (compile-values-statement subforms)
      `(,@(map js-heap-write
               (map compile-expression subforms)
               (iota (length subforms)))
        ,@(if tail-position?
              '((return))
              '())))
    (case* exp
      (let-form => compile-let-statement)
      (let-values-form => compile-let-values-statement)
      (if-form => compile-if-statement)
      (values-form => compile-values-statement)
      (_ (if tail-position?
             (let ((type (lookup-inferred-type exp)))
               (if (js-scalar-type? type)
                   `((return ,(js-coerce (compile-expression exp) type)))
                   `((statement ,(compile-expression exp))
                     (return))))
             `((statement ,(compile-expression exp)))))))
  (define (compile-expression exp)
    (case* exp
      ((fol-var var) (js-identifier var))
      ((fol-const const)
       (if (number? const)
           ;; All numbers in the program are currently floating point.
           (exact->inexact const)
           const))
      ((lambda-form _ _) (error "Escaping procedures not supported for asm.js"))
      ((pair operator operands)
       (if (js-operator? operator)
           `(,(if (< (length operands) 2)
                  'unary
                  'binary)
             ,(js-operator operator)
             ,@(map compile-expression operands))
           (js-coerce
            `(apply ,(js-identifier operator) ,(map compile-expression operands))
            (lookup-inferred-type exp))))))
  (define (local-variable-declarations body)
    (map (lambda (var)
           ;; TODO Using 'real here is wrong, but I would need
           ;; local-variable type information to fix it.
           (js-variable-declaration var 'real))
         (map js-identifier (local-names body))))
  (define compile-definition
    (rule `(define ((? name) (?? formals))
             (argument-types (?? arg-types) (? return))
             (? body))
          `(function ,(js-identifier name) ,(map js-identifier formals)
            ,@(map js-parameter-type-setter
                   (map js-identifier formals) arg-types)
            ,@(local-variable-declarations body)
            ,@(compile-statement body #t)))) ; Tail position
  (define (std-import pair)
    (let ((name (car pair))
          (source (cdr pair)))
      `(std-import ,name ,source)))
  (let ((defns (program->definitions program)))
    `(module
      fol_program
      ;; View the heap as a collection of 32-bit floats
      (view-heap heap_view "Float32Array")
      ,@(map std-import '((acos . "Math.acos")
                          (asin . "Math.asin")
                          (atan . "Math.atan2")
                          (cos  . "Math.cos")
                          (sin  . "Math.sin")
                          (tan  . "Math.tan")
                          (exp  . "Math.exp")
                          (log  . "Math.log")
                          (sqrt . "Math.sqrt")
                          (expt . "Math.pow")
                          (abs  . "Math.abs")))
      ,@(map compile-definition (filter procedure-definition? defns))
      (return __main__))))

(define (js-heap-read name index)
  `(assign ,name ,(js-coerce `(access heap_view ,index) 'real))) ; TODO Understand shifting

(define (js-heap-write expr index)
  `(assign (access heap_view ,index) ; TODO Understand shifting
           ,expr))

(define (js-scalar-type? type)
  ;; Is this a FOL type that I represent as a asm scalar?  (Other FOL
  ;; types are represented by sets of asm values, possibly on the
  ;; heap.)
  (or (eq? type 'real) (eq? type 'bool)))

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
  `(assign ,name ,(js-coerce name type)))

(define (js-variable-declaration name type)
  `(var ,name
        ,(cond ((eq? type 'real)
                0.0)
               ((eq? type 'bool)
                ;; Booleans in asm.js are integers
                0)
               (else (error "Cannot store type in asm.js variable" type)))))

(define (js-operator? thing)
  (memq thing '(+ - * / = <= >= < >)))

(define (js-operator thing)
  (if (eq? thing '=)
      '==
      thing))

(define (js-identifier thing)
  (if (eq? thing '%%main)
      '__main__
      (string-replace (display->string thing) #\- #\_)))

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
  (define-algebraic-matcher std-import-form (tagged-list? 'std-import) cadr caddr)
  (define-algebraic-matcher view-heap-form (tagged-list? 'view-heap) cadr caddr)
  (define-algebraic-matcher function-def (tagged-list? 'function) cadr caddr cdddr)
  (define-algebraic-matcher variable-declaration-form (tagged-list? 'var) cadr caddr)
  (define-algebraic-matcher assign-form (tagged-list? 'assign) cadr caddr)
  (define-algebraic-matcher if-stmt-form (tagged-list? 'if-stmt) cadr caddr cadddr)
  (define-algebraic-matcher return-void-form return-void?)
  (define-algebraic-matcher return-form (tagged-list? 'return) cadr)
  (define-algebraic-matcher statement-form (tagged-list? 'statement) cadr)
  (define-algebraic-matcher apply-form (tagged-list? 'apply) cadr caddr)
  (define-algebraic-matcher unary-form (tagged-list? 'unary) cadr caddr)
  (define-algebraic-matcher binary-form (tagged-list? 'binary) cadr caddr cadddr)
  (define-algebraic-matcher heap-access-form (tagged-list? 'access) cadr caddr)
  (define-algebraic-matcher string string? id-project)
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
      ((std-import-form name source)
       `("var " ,name " = stdlib." ,source ";" nl))
      ((view-heap-form name type)
       `("var " ,name " = new stdlib." ,type "(heap);" nl))
      ((function-def name args body)
       `("function " ,name "("
         (align
          ,@(intersperse args '("," breakable-space))) ") {" nl
         (indent
          ,@(map loop body))
         "}" nl))
      ((variable-declaration-form var exp)
       `("var " ,(loop var) " = " ,(loop exp) ";" nl))
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
      ((string var) var)
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
           (+ (string-length (display->string (car instructions)))
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
               (let ((string (display->string (car instructions))))
                 (loop
                  indent-level
                  (+ position (show string))
                  (cdr instructions)))))))))

(define (display->string thing)
  (with-output-to-string
    (lambda ()
      (display thing))))
