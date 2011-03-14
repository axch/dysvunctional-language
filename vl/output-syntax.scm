(declare (usual-integrations))
;;;; FOL (First-Order Language)

;;; FOL is a simple-minded first-order language, which serves as the
;;; compilation target for VL and DVL.  FOL has several design
;;; objectives:
;;;   FOL must be a convenient target for code generation after flow analysis
;;;   FOL must be compilable to efficient machine code
;;;   FOL should not be unduly illegible
;;; In pursuit of these design objectives, FOL is a first-oder subset
;;; of MIT Scheme, supporting a limited range of constructs.

;;; The syntax of FOL is expressed as Scheme data structures.  This
;;; may be parsed from a file or constructed directly in memory, as
;;; appropriate for the application.
;;;
;;; FOL follows the following grammar:
;;;
;;; program    = <expression>
;;;            | (begin <definition> ... <expression>)
;;;
;;; definition = (define (<proc-var> <data-var> ...) <expression>)
;;;
;;; expression = <data-var>
;;;            | <number>
;;;            | (<proc-var> <expression> ...)
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)
;;;
;;; FOL distinguishes two types of variables, one for holding
;;; procedures and one for holding data; both are represented as
;;; Scheme symbols.  The procedure variables have global scope, must
;;; be globally unique, and may only be bound by DEFINE forms.  The
;;; data variables are lexically scoped with shadowing, and may be
;;; bound by LET forms and the formal parameter positions of DEFINE
;;; forms.
;;;
;;; You may note in the above grammar several restrictions, as
;;; compared to Scheme.  Procedures may only be defined at the top
;;; level.  Procedure names must be unique.  Applications may only
;;; apply procedures directly by name --- procedures are second-class
;;; in FOL.  These restrictions are consistent with being the target
;;; language for post-flow-analysis code generation, and make FOL
;;; considerably easier to compile to efficient code.
;;;
;;; FOL comes with the following procedures predefined:
;;;
;;;   CONS, CAR, CDR, VECTOR, VECTOR-REF, ABS, EXP, LOG, SIN, COS,
;;;   TAN, ASIN, ACOS, SQRT, +, -, *, /, ATAN, EXPT, PAIR?, NULL?,
;;;   REAL?, <, <=, >, >=, =, ZERO?, POSITIVE?, NEGATIVE?, READ-REAL,
;;;   WRITE-REAL, REAL, GENSYM, GENSYM?, and GENSYM=.
;;;
;;; Those with Scheme equivalents have the same semantics.  REAL is
;;; the identity function.  READ-REAL and WRITE-REAL do i/o;
;;; WRITE-REAL returns the number written.  GENSYM synthesizes a fresh
;;; object that is guaranteed to be distinct from all other objects.
;;; GENSYM? tests whether an object was created by GENSYM.  GENSYM=
;;; tests whether two objects were created by the same dynamic call to
;;; GENSYM.
;;;
;;; FOL is specified to be tail-recursive and memory-managed.  Order
;;; of evaluation of procedure arguments is unspecified.  As of the
;;; present writing, the compilation may fail to preserve the order
;;; (or even presence!) of side-effects.  FOL is notionally strict,
;;; but the compilation may rearrange the order of evaluation of
;;; various subexpressions, even across procedure boundaries.  TODO is
;;; the dead variable elimination good enough to make the same
;;; termination guarantees that laziness does?

;;; Being a subset of Scheme, FOL can be executed directly in MIT
;;; Scheme (in an image that contains definitions for the non-standard
;;; primitives --- loading this code provides the necessary runtime
;;; support).
;;;
;;; I also provide a FOL->FOL optimizing compiler called
;;; PRETTIFY-COMPILER-OUTPUT.  This has three main pieces, each of
;;; which can also be applied separately from the others: aggressive
;;; procedure inlining, aggressive replacement of aggregates with
;;; scalars, and elimination of unused or once-used variables.
;;; Details elsewhere.
;;;
;;; I have not yet experimented with compiling FOL to any other target
;;; language.  Obvious candidates include compilation to machine code
;;; or C with the MIT Scheme compiler, or direct custom translation to
;;; C (or C--) for further processing with, e.g., gcc.  The latter
;;; activity may not be a trivial variation on the former, because in
;;; so doing the semantics of FOL may perhaps be fruitfully made
;;; somewhat different from Scheme semantics, particularly with
;;; respect to the numeric tower.

;;; TODO Discuss FOL type declarations.

;;;; Syntax and manipulations of the output language

(define let-form? (tagged-list? 'let))

(define ->lambda
  (rule `(let (? bindings) (?? body))
        `((lambda ,(map car bindings)
            ,@body)
          ,@(map cadr bindings))))

(define ->let
  (rule `((lambda (? names) (?? body)) (?? args))
        `(let ,(map list names args) ,@body)))

(define reconstitute-definition
  (iterated
   (rule `(define (? name)
            (lambda (? names)
              (?? body)))
         `(define (,name ,@names)
            ,@body))))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
           (memq (car exp) '(cons vector real car cdr vector-ref))
           (every constructors-only? (cdr exp)))))

(define (occurs-in-tree? thing tree)
  (cond ((equal? thing tree) #t)
        ((pair? tree)
         (or (occurs-in-tree? thing (car tree))
             (occurs-in-tree? thing (cdr tree))))
        (else #f)))

(define (filter-map-tree proc tree)
  (let walk ((tree tree) (answer '()))
    (if (pair? tree)
        (walk (car tree) (walk (cdr tree) answer))
        (let ((elt (proc tree)))
          (if elt
              (cons elt answer)
              answer)))))

(define (count-free-occurrences name exp)
  (cond ((eq? exp name) 1)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (lambda-formal exp))
             0
             (count-free-occurrences name (cddr exp))))
        ((let-form? exp)
         (count-free-occurrences name (->lambda exp)))
        ((pair-form? exp)
         (+ (count-free-occurrences name (car-subform exp))
            (count-free-occurrences name (cdr-subform exp))))
        ((pair? exp)
         (+ (count-free-occurrences name (car exp))
            (count-free-occurrences name (cdr exp))))
        (else 0)))

(define (replace-free-occurrences name new exp)
  (cond ((eq? exp name) new)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (lambda-formal exp))
             exp
             `(lambda ,(lambda-formal exp)
                ,@(replace-free-occurrences name new (cddr exp)))))
        ((let-form? exp)
         (->let (replace-free-occurrences name new (->lambda exp))))
        ((pair-form? exp)
         `(cons ,(replace-free-occurrences name new (car-subform exp))
                ,(replace-free-occurrences name new (cdr-subform exp))))
        ((pair? exp)
         (cons (replace-free-occurrences name new (car exp))
               (replace-free-occurrences name new (cdr exp))))
        (else exp)))

(define (alpha-extend env names)
  (append
   (map cons
        names
        (map (lambda (name)
               (if (assq name env)
                   (make-name (symbol name '-))
                   name))
             names))
   env))

(define (alpha-rename exp env)
  (cond ((assq exp env) => cdr)
        ((lambda-form? exp)
         (let* ((names (cadr exp))
                (body (cddr exp))
                (new-env (alpha-extend env names))
                (new-names (map (lambda (name)
                                  (cdr (assq name new-env)))
                                names)))
           `(lambda ,new-names
              ,@(alpha-rename body new-env))))
        ((let-form? exp)
         (->let (alpha-rename (->lambda exp) env)))
        ((definition? exp)
         ;; Assume the definiendum is already unique
         (reconstitute-definition
          `(define ,(definiendum exp)
             ,(alpha-rename (definiens exp) env))))
        ((pair? exp)
         (cons (alpha-rename (car exp) env) (alpha-rename (cdr exp) env)))
        (else exp)))
