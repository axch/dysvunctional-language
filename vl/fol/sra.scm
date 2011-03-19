(declare (usual-integrations))

;;; To do SRA, I have to recur down the expressions, and for each
;;; variable, keep track of its shape and the set of names assigned to
;;; hold its meaningful values.  The shape is useful for transforming
;;; accesses to variables, because the access can become returning the
;;; pile of names associated with that portion of the value.  This
;;; process is simplest to carry out in A-normal form or something,
;;; where every subexpression has a definite name.  Of course, the
;;; language which is the target of this must have multiple value
;;; binding and multiple value returns.

;;; In this scenario, a construction becomes a values of the appended
;;; names for the things being constructed (which I have, because of
;;; the ANF); an access becomes a values of an appropriate slice of
;;; the names being accessed (which I again have because of the ANF);
;;; A call becomes applied to the append of the names for each former
;;; element in the call (ANF strikes again); a name becomes a values
;;; of those names; a constant remains a constant; and a let becomes a
;;; multi-value let (I can invent the names for that name at this
;;; point); a definition becomes a definition taking the appropriately
;;; larger number of arguments, whose internal names I can invent at
;;; this point.  The toplevel is transformed without any initial name
;;; bindings.  The way unions interact with this is that they may
;;; cause the creation of types that are "primitive" as far as the SRA
;;; process in concerned, while being "compound" in the actual
;;; underlying code.

;;; The grammar of FOL after ANF is
;;;
;;; simple-expression = <data-var>
;;;                   | <number>
;;;
;;; expression = <simple-expression>
;;;            | (<proc-var> <simple-expression> ...)
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)

(define (empty-env) '())
(define (augment-env env old-names name-sets shapes)
  (append (map list old-names name-sets shapes)
          env))
(define (get-shape name env)
  (let ((binding (assq name env)))
    (caddr binding)))
(define (get-names name env)
  (let ((binding (assq name env)))
    (cadr binding)))
(define (count-meaningful-parts shape)
  (cond ((null? shape) 0)
        ((primitive-shape? shape) 1)
        (else (reduce + 0 (map count-meaningful-parts (sra-parts shape))))))
(define (primitive-shape? shape)
  (memq shape '(real bool gensym)))
(define (primitive-fringe shape)
  (cond ((null? shape) '())
        ((primitive-shape? shape) (list shape))
        (else (append-map primitive-fringe (sra-parts shape)))))
(define (sra-parts shape)
  ;; shape better be (cons a b) or (vector a ...)
  (cdr shape))
(define (invent-names-for-parts basename shape)
  (map (lambda (i) (make-name basename))
       (iota (count-meaningful-parts shape))))
(define (append-values values-forms)
  `(values ,@(append-map cdr values-forms)))
(define (construct-shape subshapes template)
  `(,(car template) ,@subshapes))
(define (slice-values-by-access values-form old-shape access-form)
  (cond ((eq? (car access-form) 'car)
         `(values ,@(take (cdr values-form)
                          (count-meaningful-parts (cadr old-shape)))))
        ((eq? (car access-form) 'cdr)
         `(values ,@(drop (cdr values-form)
                          (count-meaningful-parts (cadr old-shape)))))
        ((eq? (car access-form) 'vector-ref)
         (let loop ((index-left (caddr access-form))
                    (names-left (cdr values-form))
                    (shape-left (cdr old-shape)))
           (if (= 0 index-left)
               `(values ,@(take names-left
                                (count-meaningful-parts (car shape-left))))
               (loop (- index-left 1)
                     (drop names-left
                           (count-meaningful-parts (car shape-left)))
                     (cdr shape-left)))))))
(define (select-from-shape-by-access old-shape access-form)
  (cond ((eq? (car access-form) 'car)
         (cadr old-shape))
        ((eq? (car access-form) 'cdr)
         (caddr old-shape))
        ((eq? (car access-form) 'vector-ref)
         (list-ref (cdr old-shape) (caddr access-form)))))
(define (reconstruct-pre-sra-shape new-expr shape)
  (if (primitive-shape? shape)
      new-expr
      (let ((piece-names (invent-names-for-parts 'receipt shape)))
        (tidy-let-values
         `(let-values ((,piece-names ,new-expr))
            ,(let walk ((shape shape)
                        (names piece-names)
                        (win (lambda (shape new-names)
                               (assert (null? new-names))
                               shape)))
               (cond ((primitive-shape? shape)
                      (win (car names) (cdr names)))
                     ((null? shape)
                      (win '() names))
                     ((eq? 'cons (car shape))
                      (walk (cadr shape) names
                       (lambda (car-expr names-left)
                         (walk (caddr shape) names-left
                          (lambda (cdr-expr names-left)
                            (win `(cons ,car-expr ,cdr-expr)
                                 names-left))))))
                     ((eq? 'vector (car shape))
                      (let walk* ((args-left (cdr shape))
                                  (names-left names)
                                  (win (lambda (new-args names-left)
                                         (win `(vector ,@new-args) names-left))))
                        (if (null? args-left)
                            (win '() names-left)
                            (walk (car args-left) names-left
                             (lambda (new-arg names-left)
                               (walk* (cdr args-left) names-left
                                (lambda (new-args names-left)
                                  (win (cons new-arg new-args) names-left))))))))
                     (else
                      (error "Weird shape" shape)))))))))

(define (sra-expression expr env lookup-type win)
  ;; An SRA environment is not like a normal environment.  This
  ;; environment maps every bound name to two things: the shape it had
  ;; before SRA and the list of names that have been assigned by SRA
  ;; to hold its primitive parts.  The list is parallel to the fringe
  ;; of the shape.  Note that the compound structure (vector) has an
  ;; empty list of primitive parts.
  ;; The win continuation accepts the new, SRA'd expression, and the
  ;; shape of the value it used to return before SRA.
  (define (lookup-return-type thing)
    (return-type (lookup-type thing)))
  (define (lookup-arg-types thing)
    (arg-types (lookup-type thing)))
  (define (loop expr env win)
    (cond ((symbol? expr)
           (win `(values ,@(get-names expr env))
                (get-shape expr env)))
          ((number? expr)
           (win `(values ,expr) 'real))
          ((boolean? expr)
           (win `(values ,expr) 'bool))
          ((null? expr)
           (win `(values) '()))
          ((if-form? expr)
           (loop (cadr expr) env
            (lambda (new-pred pred-shape)
              ;; TODO Pred-shape better be a boolean
              (loop (caddr expr) env
               (lambda (new-cons cons-shape)
                 (loop (cadddr expr) env
                  (lambda (new-alt alt-shape)
                    ;; TODO cons-shape and alt-shape better be the same
                    ;; (or at least compatible)
                    (win `(if ,new-pred ,new-cons ,new-alt)
                         cons-shape))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop* (map cadr bindings) env
                  (lambda (new-bind-expressions bind-shapes)
                    (let ((new-name-sets
                           (map invent-names-for-parts
                                (map car bindings) bind-shapes)))
                      (loop body (augment-env
                                  env (map car bindings)
                                  new-name-sets bind-shapes)
                       (lambda (new-body body-shape)
                         (win (tidy-let-values
                               `(let-values ,(map list new-name-sets
                                                  new-bind-expressions)
                                  ,new-body))
                              body-shape))))))
                 (error "Malformed LET" expr))))
          ((accessor? expr)
           (loop (cadr expr) env
            (lambda (new-cadr cadr-shape)
              (assert (values-form? new-cadr))
              (win (slice-values-by-access new-cadr cadr-shape expr)
                   (select-from-shape-by-access cadr-shape expr)))))
          ((construction? expr)
           (loop* (cdr expr) env
            (lambda (new-terms terms-shapes)
              (assert (every values-form? new-terms))
              (win (append-values new-terms)
                   (construct-shape terms-shapes expr)))))
          (else ;; general application
           (loop* (cdr expr) env
            (lambda (new-args args-shapes)
              (assert (every values-form? new-args))
              ;; The type checker should have ensured this
              ;(assert (every equal? args-shapes (lookup-arg-types (car expr))))
              (win `(,(car expr) ,@(cdr (append-values new-args)))
                   (lookup-return-type (car expr))))))))
  (define (loop* exprs env win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) env
         (lambda (new-expr expr-shape)
           (loop* (cdr exprs) env
            (lambda (new-exprs expr-shapes)
              (win (cons new-expr new-exprs)
                   (cons expr-shape expr-shapes))))))))
  (loop expr env (lambda (new-expr shape) (win (tidy-values new-expr) shape))))

(define-structure (function-type (constructor function-type))
  args
  return)

(define return-type function-type-return)
(define arg-types function-type-args)

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
    ;; Type testers real? gensym? null? pair? have other types
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
                type-map name (function-type (map cadr args) return)))
         (cdr program))
        'ok)
    (define (lookup-type name)
      (let ((answer (hash-table/get type-map name #f)))
        (or answer
            (error "Looking up unknown name" name))))
    lookup-type))

(define (sra-program program)
  (let ((lookup-type (type-map program)))
    (define (sra-entry-point expression)
      (sra-expression
       expression (empty-env) lookup-type reconstruct-pre-sra-shape))
    (if (begin-form? program)
        (append
         (map
          (rule `(define ((? name ,symbol?) (?? formals))
                   (argument-types (?? args) (? return))
                   (? body))
                (let* ((arg-shapes (map cadr args))
                       (new-name-sets
                        (map invent-names-for-parts formals arg-shapes))
                       (env (augment-env
                             (empty-env) formals new-name-sets arg-shapes))
                       (new-names (apply append new-name-sets)))
                  `(define (,name ,@new-names)
                     (argument-types
                      ,@(map list new-names
                             (append-map primitive-fringe arg-shapes))
                      ,(tidy-values `(values ,@(primitive-fringe return))))
                     ,(sra-expression body env lookup-type
                                      (lambda (new-body shape) new-body)))))
          (except-last-pair program))
         (list (sra-entry-point (car (last-pair program)))))
        (sra-entry-point program))))

;;; The grammar of FOL after SRA is
;;;
;;; simple-expression = <data-var>
;;;                   | <number>
;;;
;;; expression = (values <simple-expression> ...)
;;;            | (<proc-var> <simple-expression> ...)
;;;            | (if <expression> <expression> <expression>)
;;;            | (let-values (((<data-var> ...) <expression>) ...) <expression>)
;;;
;;; A VALUES expression is always in tail position with repect to a
;;; matching LET-VALUES expression (except if it's emitting a boolean
;;; into the predicate position of an IF).  A <data-var> may only
;;; contain a primitive type of object.  CONS, CAR, CDR, VECTOR, and
;;; VECTOR-REF do not occur.

(define tidy-values
  (rule-simplifier
   (list
    ;; Grr, sentinel values.
    (lambda (exp)
      (if (and (values-form? exp)
               (= 2 (length exp)))
          (cadr exp)
          exp)))))

(define tidy-let-values
  (iterated
   (rule-list
    (list
     (rule `(let-values () (? body))
           body)
     (rule `(let-values ((?? bindings1)
                         (() (? exp))
                         (?? bindings2))
              (?? body))
           `(let-values (,@bindings1
                         ,@bindings2)
              ,@body))
     (rule `(let-values ((?? bindings)
                         (((? name ,symbol?)) (? exp)))
              (?? body))
           `(let-values ,bindings
              (let ((,name ,exp))
               ,@body)))
     (rule `(let-values ((?? bindings)
                         (? binding1)
                         (? binding2))
              (?? body))
           `(let-values (,@bindings
                         ,binding1)
              (let-values (,binding2)
                ,@body)))))))

;;; The post-processor above is necessary for compatibility with MIT
;;; Scheme semantics for VALUES and primitives (namely that primitives
;;; return objects, and an object is not auto-coerced to (VALUES
;;; <object>)).  However, it requires that the forms it operates on be
;;; alpha renamed.  It splits LET-VALUES to all be in series rather
;;; than in parallel.

;;; The grammar of FOL after tidying and compatibility with MIT Scheme is
;;;
;;; simple-expression = <data-var>
;;;                   | <number>
;;;
;;; expression = <simple-expression>
;;;            | (values <simple-expression> ...)
;;;            | (<proc-var> <simple-expression> ...)
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)
;;;            | (let-values (((<data-var> ...) <expression>)) <expression>)
;;;
;;; A VALUES expression is always in tail position with repect to a
;;; matching LET-VALUES expression.  A non-VALUES simple expression is
;;; always in tail position with respect to a matching LET expression.
;;; Note that now each LET-VALUES may only bind one binding (which may
;;; have multiple bound names, but only one expression).
