(declare (usual-integrations))

(define begin-form? (tagged-list? 'begin))

(define (accessor? expr)
  (or (cons-ref? expr)
      (vector-ref? expr)))
(define (cons-ref? expr)
  (and (pair? expr) (pair? (cdr expr)) (null? (cddr expr))
       (memq (car expr) '(car cdr))))
(define (vector-ref? expr)
  (and (pair? expr) (pair? (cdr expr)) (pair? (cddr expr)) (null? (cdddr expr))
       (eq? (car expr) 'vector-ref) (number? (caddr expr))))
(define (construction? expr)
  (and (pair? expr)
       (memq (car expr) '(cons vector))))
(define (push-access expr1 expr2)
  `(,(car expr1) ,expr2 ,@(cddr expr1)))

(define (sra-anf expr)
  (define (rename-nontrivial-expression expr win)
    (cond ((symbol? expr) (win expr '()))
          ((number? expr) (win expr '()))
;;           ((accessor? expr)
;;            (rename-nontrivial-subexpressions
;;             (cadr expr)
;;             (lambda (result names)
;;               (win (push-access expr result) names))))
;;           ((construction? expr)
;;            (rename-nontrivial-expressions
;;             (cdr expr)
;;             (lambda (results names)
;;               (win (cons (car expr) results) names))))
          (else
           (let ((name (make-name 'anf)))
             (win name `((,name ,expr)))))))
  (define (rename-nontrivial-expressions exprs win)
    (if (null? exprs)
        (win '() '())
        (rename-nontrivial-expression
         (car exprs)
         (lambda (result names)
           (rename-nontrivial-expressions (cdr exprs)
            (lambda (results more-names)
              (win (cons result results)
                   (append names more-names))))))))
  (let loop ((expr expr))
    (cond ((symbol? expr) expr)
          ((number? expr) expr)
          ((null? expr) expr)
          ((if-form? expr)
           `(if ,(loop (cadr expr))
                ,(loop (caddr expr))
                ,(loop (cadddr expr))))
          ((let-form? expr)
           (if (null? (cdddr expr))
               `(let ,(map (lambda (binding)
                             `(,(car binding) ,(loop (cadr binding))))
                           (cadr expr))
                  ,(loop (caddr expr)))
               (error "Malformed LET" expr)))
          ((begin-form? expr)
           (map loop expr))
          ((definition? expr)
           ((rule `(define (? formals)
                     (argument-types (?? stuff))
                     (? body))
                  `(define ,formals
                     (argument-types ,@stuff)
                     ,(loop body)))
            expr))
          (else ; application
           (rename-nontrivial-expressions
            expr
            (lambda (results names)
              (if (not (null? names))
                  (loop `(let ,names ,results))
                  expr)))))))

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

(define (sra-expression expr env lookup-type)
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
                         (win (tidy-letrec
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
  (tidy-values
   (loop expr env (lambda (new-expr shape)
                    ;; Could match the shape to the externally known
                    ;; type, if desired.
                    new-expr))))

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
      ;; TODO Reconstruct the shape that the entry point was supposed
      ;; to return?
      (sra-expression expression (empty-env) lookup-type))
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
                     ,(sra-expression body env lookup-type))))
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

(define post-sra-tidy
  (rule-simplifier
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
    (rule `(values (? exp))
          exp)
    (rule `(let-values ((? binding1)
                        (? binding2)
                        (?? bindings))
             (?? body))
          `(let-values (,binding1)
             (let-values (,binding2
                          ,@bindings)
               ,@body)))
    (rule `(let-values ((((? name ,symbol?)) (? exp)))
             (?? body))
          `(let ((,name ,exp))
             ,@body)))))

(define tidy-values
  (rule-simplifier
   (list
    (rule `(values (? exp))
          exp))))

(define tidy-letrec
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

(define values-form? (tagged-list? 'values))
(define let-values-form? (tagged-list? 'let-values))

(define (de-alias-expression expr env)
  ;; An alias environment is not like a normal environment.  This
  ;; environment maps every bound name to whether it is an alias or
  ;; not; the latter case is represented by binding the variable to
  ;; itself.  It is important to know when such an environment does
  ;; not bind a variable at all; that means that variable in not in
  ;; scope here.  For purposes of this process, (constant) numbers are
  ;; legitimate things that variables may be aliases of.
  (define (augment-env env old-names aliases win)
    (define (acceptable-alias? alias)
      (and (not (non-alias? alias))
           (or (number? alias)
               (lookup alias env))))
    (let ((aliases (if (non-alias? aliases)
                       (make-list (length old-names) the-non-alias)
                       aliases)))
      (win
       (append
        (map (lambda (old-name alias)
               (if (acceptable-alias? alias)
                   (cons old-name alias)
                   (cons old-name old-name)))
             old-names
             aliases)
        env)
       (map acceptable-alias? aliases))))
  (define lookup assq)
  (define the-non-alias (list 'not-an-alias))
  (define (non-alias? thing)
    (eq? the-non-alias thing))
  (define (merge-name-lists names1 names2)
    (if (or (non-alias? names1) (non-alias? names2))
        the-non-alias
        (map (lambda (name1 name2)
               (if (eq? name1 name2)
                   name1
                   the-non-alias))
             names1 names2)))
  ;; The win continuation accepts the new, de-aliased expression, and
  ;; a list of the names of the variables that hold the return values
  ;; from this expression.
  (define (loop expr env win)
    (cond ((symbol? expr)
           (let ((alias-binding (lookup expr env)))
             (if alias-binding
                 (win (cdr alias-binding) (list (cdr alias-binding)))
                 (error "Trying to de-alias an unbound variable" expr env))))
          ((number? expr)
           (win expr (list expr)))
          ((values-form? expr)
           (loop* (cdr expr) env
            (lambda (exprs names-lists)
              (win `(values ,@exprs) (apply append names-lists)))))
          ((if-form? expr)
           (loop (cadr expr) env
            (lambda (new-pred pred-names)
              ;; TODO Pred-shape better be a boolean
              (loop (caddr expr) env
               (lambda (new-cons cons-names)
                 (loop (cadddr expr) env
                  (lambda (new-alt alt-names)
                    (win `(if ,new-pred ,new-cons ,new-alt)
                         (merge-name-lists cons-names alt-names)))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop* (map cadr bindings) env
                  (lambda (new-bind-expressions bind-name-lists)
                    (let ((bind-names
                           (map (lambda (bind-name-list)
                                  ;; These better all be singletons
                                  (if (non-alias? bind-name-list)
                                      the-non-alias
                                      (car bind-name-list)))
                                bind-name-lists)))
                      (augment-env env (map car bindings) bind-names
                       (lambda (env acceptable-aliases)
                         (loop body env
                          (lambda (new-body body-name-list)
                            (win (empty-let-rule
                                  `(let ,(filter-map
                                          (lambda (name alias? expr)
                                            (and (not alias?)
                                                 (list name expr)))
                                          (map car bindings)
                                          acceptable-aliases
                                          new-bind-expressions)
                                     ,new-body))
                                 body-name-list))))))))
                 (error "Malformed LET" expr))))
          ((let-values-form? expr)
           (let* ((binding (caadr expr))
                  (names (car binding))
                  (subexpr (cadr binding))
                  (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop subexpr env
                  (lambda (new-subexpr subexpr-names)
                    (augment-env env names subexpr-names
                     (lambda (env acceptable-aliases)
                       (loop body env
                        (lambda (new-body body-name-list)
                          ;; ACCEPTABLE-ALIASES tells me which of
                          ;; these bindings are guaranteed to be dead
                          ;; because the variables being bound are
                          ;; aliases and have already been replaced in
                          ;; the new body.  I could eliminate them,
                          ;; but that would require traversing subexpr
                          ;; again to look for the VALUES that
                          ;; supplies the corresponding values.  For
                          ;; now, I will just kill the whole
                          ;; let-values if it is useless.
                          (win (if (any not acceptable-aliases)
                                   `(let-values ((,names ,new-subexpr))
                                      ,new-body)
                                   new-body)
                               body-name-list)))))))
                 (error "Malformed LET-VALUES" expr))))
          (else ;; general application
           (loop* (cdr expr) env
            (lambda (new-args args-names-lists)
              (win `(,(car expr) ,@new-args)
                   the-non-alias))))))
  (define (loop* exprs env win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) env
         (lambda (new-expr expr-names)
           (loop* (cdr exprs) env
            (lambda (new-exprs expr-names-lists)
              (win (cons new-expr new-exprs)
                   (cons expr-names expr-names-lists))))))))
  (loop expr env (lambda (new-expr name-list)
                   ;; The name list might be useful to an
                   ;; interprocedural must-alias crunch.
                   new-expr)))

(define (intraprocedural-de-alias program)
  (if (begin-form? program)
      (append
       (map
        (rule `(define ((? name ,symbol?) (?? formals))
                 (argument-types (?? stuff))
                 (? body))
              `(define (,name ,@formals)
                 (argument-types ,@stuff)
                 ,(de-alias-expression body (map cons formals formals))))
        (except-last-pair program))
       (list (de-alias-expression (car (last-pair program)) '())))
      (de-alias-expression program '())))

;;; I don't (yet) even want to think about what it would take to do a
;;; good job of interprocedural de-aliasing.

(define (expression-dead-variable-elimination expr live-out)
  (define (ignore? name)
    (eq? name '_))
  (define (no-used-vars) '())
  (define (single-used-var var) (list var))
  (define (union vars1 vars2)
    (lset-union eq? vars1 vars2))
  (define (difference vars1 vars2)
    (lset-difference eq? vars1 vars2))
  (define used? memq)
  (define (loop expr live-out win)
    (cond ((symbol? expr)
           (win expr (single-used-var expr)))
          ((number? expr)
           (win expr (no-used-vars)))
          ((if-form? expr)
           (let ((predicate (cadr expr))
                 (consequent (caddr expr))
                 (alternate (cadddr expr)))
             (loop predicate #t
              (lambda (new-predicate pred-used)
                (loop consequent live-out
                 (lambda (new-consequent cons-used)
                   (loop alternate live-out
                    (lambda (new-alternate alt-used)
                      (win `(if ,new-predicate
                                ,new-consequent
                                ,new-alternate)
                           (union pred-used (union cons-used alt-used)))))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (loop body live-out
              (lambda (new-body body-used)
                (let ((new-bindings
                       (filter (lambda (binding)
                                 (used? (car binding) body-used))
                               bindings)))
                  (loop* (map cadr new-bindings)
                   (lambda (new-exprs exprs-used)
                     (let ((used (reduce union (no-used-vars) exprs-used)))
                       (win (empty-let-rule
                             `(let ,(map list (map car new-bindings)
                                         new-exprs)
                                ,new-body))
                            (union used (difference
                                         body-used (map car bindings))))))))))))
          ((let-values-form? expr)
           (let ((binding (caadr expr))
                 (body (caddr expr)))
             (let ((names (car binding))
                   (sub-expr (cadr binding)))
               (loop body live-out
                (lambda (new-body body-used)
                  (define (slot-used? name)
                    (or (ignore? name)
                        (and (used? name body-used)
                             #t)))
                  (let ((sub-expr-live-out (map slot-used? names)))
                    (if (any (lambda (x) x) sub-expr-live-out)
                        (loop sub-expr sub-expr-live-out
                         (lambda (new-sub-expr sub-expr-used)
                           (win (tidy-letrec
                                 `(let-values ((,(filter slot-used? names)
                                                ,new-sub-expr))
                                    ,new-body))
                                (union sub-expr-used
                                       (difference body-used names)))))
                        (win new-body body-used))))))))
          ;; If used post SRA, there may be constructions to build the
          ;; answer for the outside world, but there should be no
          ;; accesses.
          ((construction? expr)
           (loop* (cdr expr)
            (lambda (new-args args-used)
              (win `(,(car expr) ,@new-args)
                   (reduce union (no-used-vars) args-used)))))
          ((values-form? expr)
           ;; TODO The invariant that the LIVE-OUT of a VALUES form is
           ;; always a list holds good only if the FOL entry point
           ;; never tries to return a VALUES frob.  For that claim to
           ;; be true, I need to explicitly reconstruct the
           ;; externally-desired shape from any VALUES frob that SRA
           ;; might make.
           (if (not (list? live-out))
               (set! live-out (make-list (length (cdr expr)) #t)))
           ;(assert (list? live-out))
           (let ((wanted-elts (filter-map (lambda (wanted? elt)
                                            (and wanted? elt))
                                          live-out
                                          (cdr expr))))
             (loop* wanted-elts
              (lambda (new-elts elts-used)
                (win (if (= 1 (length new-elts))
                         (car new-elts)
                         `(values ,@new-elts))
                     (reduce union (no-used-vars) elts-used))))))
          (else ;; general application
           (loop* (cdr expr)
            (lambda (new-args args-used)
              (define (all-wanted? live-out)
                (or (equal? live-out #t)
                    (every (lambda (x) x) live-out)))
              (define (invent-name wanted?)
                (if wanted?
                    (make-name 'receipt)
                    '_))
              (let ((simple-new-call `(,(car expr) ,@new-args)))
                (let ((new-call
                       (if (all-wanted? live-out)
                           simple-new-call
                           (let ((receipt-names (map invent-name live-out)))
                             `(let-values ((,receipt-names ,simple-new-call))
                                (values ,@(filter (lambda (x) (not (ignore? x)))
                                                  receipt-names)))))))
                  (win new-call (reduce union (no-used-vars) args-used)))))))))
  (define (loop* exprs win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) #t
         (lambda (new-expr expr-used)
           (loop* (cdr exprs)
            (lambda (new-exprs exprs-used)
              (win (cons new-expr new-exprs)
                   (cons expr-used exprs-used))))))))
  (loop expr live-out (lambda (new-expr used-vars) new-expr)))

(define (intraprocedural-dead-variable-elimination program)
  (if (begin-form? program)
      (append
       (map
        (rule `(define ((? name ,symbol?) (?? formals))
                 (argument-types (?? stuff) (? return))
                 (? body))
              `(define (,name ,@formals)
                 (argument-types ,@stuff)
                 ,(expression-dead-variable-elimination
                   body (or (not (values-form? return))
                            (map (lambda (x) #t) (cdr return))))))
        (except-last-pair program))
       (list (expression-dead-variable-elimination
              (car (last-pair program)) #t)))
      (expression-dead-variable-elimination program #t)))

;;; To do interprocedural dead variable elimination I have to proceed
;;; as follows:
;;; -1) Run a round of intraprocedural dead variable elimination to
;;;     diminish the amount of work in the following (assume all
;;;     procedure calls need all their inputs)
;;; 0) Treat the final expression as a nullary procedure definition
;;; 1) Initialize a map for each procedure, mapping from output that
;;;    might be desired (by index) to input that is known to be needed
;;;    to compute that output.
;;;    - I know the answer for primitives
;;;    - All compound procedures start mapping every output to the empty
;;;      set of inputs known to be needed.
;;; 2) I can improve the map by walking the body of a procedure, carrying
;;;    down the set of desired outputs and bringing up the map saying
;;;    which outputs require which inputs
;;;    - Start with all outputs desired.
;;;    - A number requires no inputs for one output
;;;    - A variable requires itself for one output
;;;    - A VALUES maps its subexpressions to the desired outputs
;;;    - A LET is transparent on the way down, but if the variable it
;;;      is binding is desired as an input to its body, it recurs on
;;;      its expression desiring the one output.  Whatever input come
;;;      up need to be spliced in to the answers in the map coming from
;;;      the body.
;;;    - A LET-VALUES is analagous, but may choose to desire a subset
;;;      of its bound names.
;;;    - An IF recurs on the predicate desiring its output, and then
;;;      on the consequent and alternate passing the requests.  When
;;;      the answers come back, it needs to union the consequent and
;;;      alternate maps, and then add the predicate requirements as
;;;      inputs to all desired outputs of the IF.
;;;    - A procedure call refers to the currently known map for that
;;;      procedure.
;;;    - Whatever comes out of the top becomes the new map for this
;;;      procedure.
;;; 3) Repeat step 2 until no more improvements are possible.
;;; 4) Initialize a table of which inputs and outputs to each compound
;;;    procedure are actually needed.
;;;    - All procedures start not needed
;;;    - The entry point starts fully needed
;;; 5) I can improve this table by walking the body of a procedure
;;;    some of whose outputs are needed, carrying down the set of outputs
;;;    that are needed and bringing back up the set of inputs that are needed.
;;;    - At a procedure call, mark outputs of that procedure as needed in
;;;      the table if I found that I needed them on the walk; then take back
;;;      up the set of things that that procedure says it needs.
;;;    - Otherwise walk as in step 2 (check this!)
;;; 6) Repeat step 5 until no more improvements are possible.
;;; 7) Replace all definitions to
;;;    - Accept only those arguments they need (internally LET-bind all
;;;      others to tombstones)
;;;    - Return only those outputs that are needed (internally
;;;      LET-VALUES everything the body will generate, and VALUES out
;;;      that which is wanted)
;;; 8) Replace all call sites to
;;;    - Supply only those arguments that are needed (just drop
;;;      the rest)
;;;    - Solicit only those outputs that are needed (LET-VALUES them,
;;;      and VALUES what the body expects, filling in with tombstones).
;;; 9) Run a round of intraprocedural dead variable elimination to
;;;    clean up (all procedure calls now do need all their inputs)
;;;    - Verify that all the tombstones vanish.

;; This is safe assuming the program has been alpha renamed
(define values-let-lifting-rule
  (rule `(let-values (((? names) (let (? in-bindings) (? exp))))
           (?? body))
        `(let ,in-bindings
           (let-values ((,names ,exp))
             ,@body))))

(define post-hair-tidy
  (rule-simplifier
   (list
    (rule `(begin (? body)) body)
    (rule `(* 0 (? thing)) 0)
    (rule `(* (? thing) 0) 0)
    (rule `(+ 0 (? thing)) thing)
    (rule `(+ (? thing) 0) thing)
    (rule `(* 1 (? thing)) thing)
    (rule `(* (? thing) 1) thing)

    (rule `(if (? predicate) (? exp) (? exp))
          exp)

    (rule `(let-values (((? names) (values (?? stuff))))
             (?? body))
          `(let ,(map list names stuff)
             ,@body))

    empty-let-rule

    values-let-lifting-rule

    (rule `(let ((?? bindings1)
                 ((? name ,symbol?) (? exp))
                 (?? bindings2))
             (?? body))
          (let ((occurrence-count (count-free-occurrences name body)))
            (and (= 1 occurrence-count)
                 `(let (,@bindings1
                        ,@bindings2)
                    ,@(replace-free-occurrences name exp body))))))))

(define (hairy-optimize output)
  (if (list? output)
      ((lambda (x) x) ; This makes the last stage show up in the stack sampler
       (strip-argument-types
        (post-hair-tidy
         (intraprocedural-dead-variable-elimination
          (intraprocedural-de-alias
           (sra-program
            (sra-anf
             (full-alpha-rename
              (inline
               (structure-definitions->vectors
                output))))))))))
      output))
