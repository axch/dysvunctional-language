(declare (usual-integrations))
;;;; Type checking

;;; FOL procedure definitions are annotated with their type
;;; signatures, but local variable bindings in FOL are not explicitly
;;; marked.  Type checking FOL is therefore not a completely local
;;; operation, but can be done via a recursive traversal of the code
;;; bodies.  The traversal carries down an environment indicating the
;;; type of every bound variable, and, if the subexpression is
;;; internally well typed, returns the type of the object said
;;; subexpression returns.

;;; In principle, much can be done to make a type checker emit good
;;; type error messages.  I did not do it.  This type checker does the
;;; worst possible thing, which is to signal an error in the enclosing
;;; MIT Scheme as soon as it finds anything wrong.  The context of
;;; such an error can be inspected by entering the MIT Scheme
;;; debugger.

;;; This type checker also doubles as a syntax checker.  In principle,
;;; I should derive the syntax checker mechanically from the FOL
;;; grammar, but instead of learning (or writing) a tool for doing
;;; that I put some basic syntax checks into this program.

(define (check-program-types program #!optional inferred-type-map)
  (define (check-definition-syntax definition)
    (if (not (definition? definition))
        (error "Non-definition in a non-terminal program position"
               definition))
    (if (not (= 4 (length definition)))
        (error "Malformed definition" definition))
    (let ((formals (cadr definition))
          (types (caddr definition)))
      (if (not (list? formals))
          (error "Malformed formals list" definition))
      (if (not (list? types))
          (error "Malformed type declaration" definition))
      (if (not (= (length types) (+ 1 (length formals))))
          (error "Type declaration not parallel to formals list"
                 definition))
      (if (memq (car formals) fol-reserved)
          (error "Defining a reserved name" (car formals)))
      (for-each
       (lambda (type sub-index)
         (if (not (fol-shape? type))
             (error "Type declaring a non-type"
                    type definition sub-index)))
       (except-last-pair (cdr types))
       (iota (length (cdr formals))))
      (check-unique-names (cdr formals) "Repeated formal parameter")))
  (if (begin-form? program)
      (begin
        (for-each check-definition-syntax
                  (except-last-pair (cdr program)))
        (check-unique-names
         (map definiendum (except-last-pair (cdr program)))
         "Repeated definition")))
  (let ((lookup-type (type-map program)))
    (define (check-definition-types definition)
      (let ((formals (cadr definition))
            (types (caddr definition))
            (body (cadddr definition)))
        (let ((body-type
               (check-expression-types
                body
                (augment-type-env! (empty-type-env) (cdr formals)
                                   (arg-types (lookup-type (car formals))))
                lookup-type
                inferred-type-map)))
          (if (not (equal? (last types) body-type))
              (error "Return type declaration doesn't match"
                     definition (last types) body-type))
          body-type)))
    (define (check-entry-point-types expression)
      (check-expression-types
       expression (empty-type-env) lookup-type inferred-type-map))
    (if (begin-form? program)
        (begin
          (for-each check-definition-types (except-last-pair (cdr program)))
          (check-entry-point-types (last program)))
        (check-entry-point-types program))))

(define (check-expression-types expr env global-type #!optional inferred-type-map)
  ;; A type environment maps every bound local name to its type.  The
  ;; global-type procedure returns the (function) type of any global
  ;; name passed to it.  CHECK-EXPRESSION-TYPES either returns the
  ;; type of the expression or signals an error if the expression is
  ;; either malformed or not type correct.
  ;; For this purpose, a VALUES is the same as any other construction,
  ;; but in other contexts they may need to be distinguished.
  (define (construction? expr)
    (and (pair? expr)
         (memq (car expr) '(cons vector values))))
  (define (loop expr env)
    (let ((type (%loop expr env)))
      (if (not (default-object? inferred-type-map))
          (hash-table/put! inferred-type-map expr type))
      type))
  (define (%loop expr env)
    (cond ((fol-var? expr) (lookup-type expr env))
          ((number? expr) 'real)
          ((boolean? expr) 'bool)
          ((null? expr) '())
          ((if-form? expr) (check-if-types expr env))
          ((let-form? expr) (check-let-types expr env))
          ((let-values-form? expr) (check-let-values-types expr env))
          ((lambda-form? expr) (check-lambda-types expr env))
          ((cons-ref? expr) (check-cons-ref-types expr env))
          ((vector-ref? expr) (check-vector-ref-types expr env))
          ((construction? expr) (check-construction-types expr env))
          (else (check-application-types expr env))))
  (define (check-if-types expr env)
    (if (not (= 4 (length expr)))
        (error "Malformed IF" expr))
    (let ((pred-type (loop (cadr expr) env))
          (cons-type (loop (caddr expr) env))
          (alt-type (loop (cadddr expr) env)))
      (if (not (eq? 'bool pred-type))
          (error "IF predicate not of boolean type" expr))
      (if (not (equal? cons-type alt-type))
          ;; Note: this place will need to change to support union types
          (error "Different IF branches return different types" expr))
      cons-type))
  (define (check-let-types expr env)
    (if (not (= 3 (length expr)))
        (error "Malformed LET (excess body forms?)" expr))
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (if (not (list? bindings))
          (error "Malformed LET (non-list bindings)" expr))
      (for-each
       (lambda (binding index)
         (if (not (list? binding))
             (error "Malformed LET binding (not a list)" bindings index))
         (if (not (= 2 (length binding)))
             (error "Malformed LET binding (not a list of length 2)" bindings index)))
       bindings
       (iota (length bindings)))
      (check-unique-names (map car bindings) "Repeated LET binding")
      (let ((binding-types
             (map (lambda (exp) (loop exp env)) (map cadr bindings))))
        (for-each
         (lambda (binding-type index)
           (if (values-form? binding-type)
               (error "LET binds a VALUES shape"
                      expr binding-type index)))
         binding-types
         (iota (length binding-types)))
        (abegin1
            (loop body (augment-type-env!
                        env (map car bindings) binding-types))
          (degment-type-env! env (map car bindings))))))
  (define (check-let-values-types expr env)
    (if (not (= 3 (length expr)))
        (error "Malformed LET-VALUES (excess body forms?)" expr))
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (if (not (list? bindings))
          (error "Malformed LET-VALUES (non-list bindings)" expr))
      (if (not (= 1 (length bindings)))
          (error "Malformed LET-VALUES (multiple binding expressions)"
                 expr))
      (if (not (list? (car bindings)))
          (error "Malformed LET-VALUES binding (not a list)" bindings))
      (if (not (= 2 (length (car bindings))))
          (error "Malformed LET-VALUES binding (not a list of length 2)" bindings))
      (check-unique-names (caar bindings) "Repeated LET-VALUES binding")
      (let ((binding-type (loop (cadar bindings) env)))
        (if (not (values-form? binding-type))
            (error "LET-VALUES binds a non-VALUES shape"
                   expr binding-type))
        (if (not (= (length (caar bindings)) (length (cdr binding-type))))
            (error "LET-VALUES binds the wrong number of VALUES"
                   expr binding-type))
        (abegin1
            (loop body (augment-type-env!
                        env (caar bindings) (cdr binding-type)))
          (degment-type-env! env (caar bindings))))))
  (define (check-lambda-types expr env)
    (if (not (= 3 (length expr)))
        (error "Malformed LAMBDA (excess body forms?)" expr))
    (let ((formal (cadr expr))
          (body (caddr expr)))
      (if (not (list? formal))
          (error "Malformed LAMBDA (formal not a list)" expr))
      (if (not (= 1 (length formal)))
          (error "Malformed LAMBDA (multiple args not allowed)" expr))
      (let ((body-type (loop body (augment-type-env! env formal (list 'real)))))
        (degment-type-env! env formal)
        ;; TODO Do I want to actually declare and check lambda expression types?
        ;; TODO Extend to checking other foreign types
        #;(function-type 'real body-type)
        'escaping-function)))
  (define (check-cons-ref-types expr env)
    (if (not (= (length expr) 2))
        (error "Malformed pair access" expr))
    (let ((accessee-type (loop (cadr expr) env)))
      (if (not (eq? 'cons (car accessee-type)))
          (if (eq? 'car (car expr))
              (error "Taking the CAR of a non-CONS" accessee-type)
              (error "Taking the CDR of a non-CONS" accessee-type)))
      (select-from-shape-by-access accessee-type expr)))
  (define (check-vector-ref-types expr env)
    (if (not (= (length expr) 3))
        (error "Malformed vector reference" expr))
    (if (not (number? (caddr expr)))
        (error "Trying to access a computed vector slot" expr))
    (let ((accessee-type (loop (cadr expr) env)))
      (if (not (eq? 'vector (car accessee-type)))
          (error "Trying to VECTOR-REF a non-VECTOR"
                 accessee-type))
      (if (not (< (caddr expr) (length (cdr accessee-type))))
          (error "Index out of bounds"
                 (caddr expr) accessee-type))
      (select-from-shape-by-access accessee-type expr)))
  (define (check-construction-types expr env)
    (let ((element-types (map (lambda (exp) (loop exp env)) (cdr expr))))
      (for-each
       (lambda (element-type index)
         (if (values-form? element-type)
             (error "Trying to put a VALUES shape into a data structure"
                    expr element-type index)))
       element-types
       (iota (length element-types)))
      (construct-shape element-types (car expr))))
  (define (check-application-types expr env)
    (if (not (fol-var? (car expr)))
        (error "Calling a statically unknown procedure" expr))
    (let ((expected-types (arg-types (global-type (car expr))))
          (argument-types (map (lambda (exp) (loop exp env)) (cdr expr))))
      (if (not (= (length expected-types) (length argument-types)))
          (error "Trying to call function with wrong number of arguments"
                 expr))
      (for-each
       (lambda (expected given index)
         (if (not (equal? expected given))
             (error "Mismatched argument at function call"
                    expr index expected given)))
       expected-types
       argument-types
       (iota (length argument-types)))
      (return-type (global-type (car expr)))))
  (loop expr env))

(define (fol-shape? thing)
  ;; This will need to be updated when union types appear
  (or (null? thing)
      (and (fol-var? thing)
           (memq thing '(real bool gensym escaping-function)))
      (and (list? thing)
           (> (length thing) 0)
           (memq (car thing) '(cons vector values))
           (or (not (eq? 'cons (car thing)))
               (= 2 (length (cdr thing))))
           (every fol-shape? (cdr thing)))))

(define (empty-type-env) (make-strong-eq-hash-table))
(define (augment-type-env! env names shapes)
  (for-each (lambda (name shape)
              (hash-table/put! env name
                               (cons shape (hash-table/get env name '()))))
            names shapes)
  env)
(define (lookup-type name env)
  (define (lose) (error "Refencing an unbound variable" name))
  (hash-table/lookup env name
   (lambda (stack) (if (null? stack) (lose) (car stack)))
   lose))
(define (degment-type-env! env names)
  (define (lose) (error "Degmenting an unbound variable" name))
  (for-each (lambda (name)
              (hash-table/lookup
               env name
               (lambda (stack)
                 (if (null? stack)
                     (lose)
                     (if (null? (cdr stack))
                         (hash-table/remove! env name)
                         (hash-table/put! env name (cdr stack)))))
               lose))
            names)
  env)

(define (check-unique-names names message)
  (pair-for-each
   (lambda (names)
     (if (memq (car names) fol-reserved)
         (error "Trying to bind reserved name" (car names)))
     (if (memq (car names) (cdr names))
         (error message (car names))))
   names))

;;; A type map maps the name of any FOL procedure to a function-type
;;; object representing its argument types and return type.  I
;;; implement this as a hash table backed procedure that returns that
;;; information when given the name in question, or #f if the given
;;; name is not the name of a global procedure.  This is useful here
;;; to type-check FOL, and will also be used for other FOL stages that
;;; need to look up type information of FOL procedures.

(define (type-map program)
  (define (make-initial-type-map)
    (alist->eq-hash-table
     (map (lambda (prim)
            (cons (primitive-name prim) (primitive-type prim)))
          *primitives*)))
  (let ((type-map (make-initial-type-map)))
    (if (begin-form? program)
        (for-each
         (rule `(define ((? name ,fol-var?) (?? formals))
                  (argument-types (?? args) (? return))
                  (? body))
               (hash-table/put!
                type-map name (function-type args return)))
         (cdr program))
        'ok)
    (define (lookup-type name)
      (or (hash-table/get type-map name #f)
          (error "Looking up unknown name" name)))
    lookup-type))

(define (equal-type? type1 type2)
  (cond ((and (function-type? type1) (function-type? type2))
         (and (equal-type? (arg-types type1) (arg-types type2))
              (equal-type? (return-type type1) (return-type type2))))
        ((and (pair? type1) (pair? type2))
         (and (equal-type? (car type1) (car type2))
              (equal-type? (cdr type1) (cdr type2))))
        (else (equal? type1 type2))))
