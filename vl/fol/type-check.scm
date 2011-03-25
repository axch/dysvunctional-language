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

(define (check-program-types program)
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
                (augment-type-env (empty-type-env) (cdr formals)
                                  (arg-types (lookup-type (car formals))))
                lookup-type)))
          (if (not (equal? (last types) body-type))
              (error "Return type declaration doesn't match"
                     definition (last types) body-type))
          body-type)))
    (define (check-entry-point-types expression)
      (check-expression-types expression (empty-type-env) lookup-type))
    (if (begin-form? program)
        (begin
          (for-each check-definition-types (except-last-pair (cdr program)))
          (check-entry-point-types (last program)))
        (check-entry-point-types program))))

(define (check-expression-types expr env global-type)
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
    (cond ((symbol? expr) (lookup-type expr env))
          ((number? expr) 'real)
          ((boolean? expr) 'bool)
          ((null? expr) '())
          ((if-form? expr) (check-if-types expr env))
          ((let-form? expr) (check-let-types expr env))
          ((let-values-form? expr) (check-let-values-types expr env))
          ((accessor? expr) (check-accessor-types expr env))
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
        (loop body (augment-type-env
                    env (map car bindings) binding-types)))))
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
      (check-unique-names (caar bindings) "Repeated LET-VALUES binding")
      (let ((binding-type (loop (cadar bindings) env)))
        (if (not (values-form? binding-type))
            (error "LET-VALUES binds a non-VALUES shape"
                   expr binding-type))
        (if (not (= (length (caar bindings)) (length (cdr binding-type))))
            (error "LET-VALUES binds the wrong number of VALUES"
                   expr binding-type))
        (loop body (augment-type-env
                    env (caar bindings) (cdr binding-type))))))
  (define (check-accessor-types expr env)
    (let ((accessee-type (loop (cadr expr) env)))
      (if (and (cons-ref? expr) (not (eq? 'cons (car accessee-type))))
          (if (eq? 'car (car expr))
              (error "Taking the CAR of a non-CONS" accessee-type)
              (error "Taking the CDR of a non-CONS" accessee-type)))
      (if (vector-ref? expr)
          (begin
            (if (not (eq? 'vector (car accessee-type)))
                (error "Trying to VECTOR-REF a non-VECTOR"
                       accessee-type))
            (if (not (< (caddr expr) (length (cdr accessee-type))))
                (error "Index out of bounds"
                       (caddr expr) accessee-type))))
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
      (construct-shape element-types expr)))
  (define (check-application-types expr env)
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
      (and (symbol? thing)
           (memq thing '(real bool gensym)))
      (and (list? thing)
           (> (length thing) 0)
           (memq (car thing) '(cons vector values))
           (or (not (eq? 'cons (car thing)))
               (= 2 (length (cdr thing))))
           (every fol-shape? (cdr thing)))))

(define (empty-type-env) '())
(define (augment-type-env env names shapes)
  (append (map list names shapes) env))
(define (lookup-type name env)
  (let ((binding (assq name env)))
    (if (not binding)
        (error "Refencing an unbound variable" name)
        (cadr binding))))

(define (check-unique-names names message)
  (pair-for-each
   (lambda (names)
     (if (memq (car names) (cdr names))
         (error message (car names))))
   names))

;;; A type map maps the name of any FOL procedure to a function-type
;;; object representing its argument types and return type.  I
;;; implement this as a hash table backed procedure that returns that
;;; information when given the name in question, or #f if the given
;;; name is not the name of a global procedure.  This is useful here
;;; to type-check FOL, and will also be used to do scalar replacement
;;; of aggregates.

(define (type-map program)
  (define (make-initial-type-map)
    (define (real->real thing)
      (cons thing (function-type '(real) 'real)))
    (define (real*real->real thing)
      (cons thing (function-type '(real real) 'real)))
    (define (real->bool thing)
      (cons thing (function-type '(real) 'bool)))
    (define (real*real->bool thing)
      (cons thing (function-type '(real real) 'bool)))
    ;; Type testers real? gensym? null? pair? have other types, but
    ;; should never be emitted by VL or DVL on union-free inputs.
    (alist->eq-hash-table
     `((read-real . ,(function-type '() 'real))
       ,@(map real->real
              '(abs exp log sin cos tan asin acos sqrt write-real real))
       ,@(map real*real->real '(+ - * / atan expt))
       ,@(map real->bool '(zero? positive? negative?))
       ,@(map real*real->bool '(< <= > >= =))
       (gensym . ,(function-type '() 'gensym))
       (gensym= . ,(function-type '(gensym gensym) 'bool)))))
  (let ((type-map (make-initial-type-map)))
    (if (begin-form? program)
        (for-each
         (rule `(define ((? name ,symbol?) (?? formals))
                  (argument-types (?? args) (? return))
                  (? body))
               (hash-table/put!
                type-map name (function-type args return)))
         (cdr program))
        'ok)
    (define (lookup-type name)
      (let ((answer (hash-table/get type-map name #f)))
        (or answer
            (error "Looking up unknown name" name))))
    lookup-type))

(define-structure (function-type (constructor function-type))
  args
  return)

(define return-type function-type-return)
(define arg-types function-type-args)
