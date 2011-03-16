(declare (usual-integrations))

;;; lifting lets needs alpha
;;;   informs inlining direct constructions
;;; inlining singletons needs alpha
;;;   informs pushing accessors
;;;     (let ((x (if ... (cons) (cons))))
;;;       (car x))
;;;   informs inlining direct constructions by let-lifting
;;;   may need to be undone by inlining direct constructions
;;; inlining direct constructions
;;;   informs pushing accessors
;;;   informs itself by let-lifting
;;; pushing accessors makes the code smaller
;;;   informs deletion of variables
;;;   informs inlining singletons
;;; deletion of variables and empty lets makes the code smaller
;;;   informs inlining direct constructions by let-lifting
;;;   informs itself
;;;   informs inlining singletons

;;; delete dead code -> alpha rename -> lift lets ->
;;; inline direct -> push -> delete -> loop? inline singletons? ->

;; Empirically, this seems to give a reduction of about 20% of pairs
;; when given (inline (structure-definitions->vectors raw-fol)).
(define (intraprocedural-dead-elimination expr)
  (define (no-used-vars) '())
  (define (single-used-var var) (list var))
  (define (union vars1 vars2)
    (lset-union eq? vars1 vars2))
  (define (difference vars1 vars2)
    (lset-difference eq? vars1 vars2))
  (define used? memq)
  (let loop ((expr expr)
             (win (lambda (new-expr used-vars) new-expr)))
    (define (loop* exprs win)
      (let loop* ((exprs exprs)
                  (finished '())
                  (used (no-used-vars)))
        (if (null? exprs)
            (win (reverse finished) used)
            (loop (car exprs)
             (lambda (new-expr expr-used)
               (loop* (cdr exprs) (cons new-expr finished)
                      (union used expr-used)))))))
    (cond ((symbol? expr)
           (win expr (single-used-var expr)))
          ((number? expr)
           (win expr (no-used-vars)))
          ((if-form? expr)
           (let ((predicate (cadr expr))
                 (consequent (caddr expr))
                 (alternate (cadddr expr)))
             (loop predicate
              (lambda (new-predicate pred-used)
                (loop consequent
                 (lambda (new-consequent cons-used)
                   (loop alternate
                    (lambda (new-alternate alt-used)
                      (win `(if ,new-predicate
                                ,new-consequent
                                ,new-alternate)
                           (union pred-used (union cons-used alt-used)))))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop body
                       (lambda (new-body body-used)
                         (let ((new-bindings
                                (filter (lambda (binding)
                                          (used? (car binding) body-used))
                                        bindings)))
                           (loop* (map cadr new-bindings)
                                  (lambda (new-exprs used)
                                    (win (empty-let-rule
                                          `(let ,(map list (map car new-bindings)
                                                      new-exprs)
                                             ,new-body))
                                         (union used (difference
                                                      body-used (map car bindings)))))))))
                 (error "Malformed LET" expr))))
          (else (loop* expr win)))))

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

;; Empirically, this has no effect without some form of construction
;; inlining.
(define (push-down-accessors expr)
  (define (empty-context) '())
  (define empty-context? null?)
  (define pop cadr)
  (define (access context expr)
    (cond ((eq? 'car (car context))
           (cadr expr))
          ((eq? 'cdr (car context))
           (caddr expr))
          ((eq? 'vector-ref (car context))
           (list-ref (cdr expr) (caddr context)))))
  (define (compatible? context expr)
    (or (and (memq (car context) '(car cdr)) (eq? (car expr) 'cons))
        (and (eq? (car context) 'vector-ref) (eq? (car expr) 'vector))))
  (define (reconstruct context expr)
    (if (empty-context? context)
        expr
        (reconstruct (pop context) (push-access context expr))))
  (let loop ((expr expr) (context (empty-context)))
    (cond ((symbol? expr)
           (reconstruct context expr))
          ((number? expr)
           (reconstruct context expr))
          ((if-form? expr)
           (let ((predicate (cadr expr))
                 (consequent (caddr expr))
                 (alternate (cadddr expr)))
             `(if ,(loop predicate (empty-context))
                  ,(loop consequent context)
                  ,(loop alternate context))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 `(let ,(map list (map car bindings)
                             (map (lambda (expr)
                                    (loop expr (empty-context)))
                                  (map cadr bindings)))
                    ,(loop body context))
                 (error "Malformed LET" expr))))
          ((and (not (empty-context? context)) (construction? expr))
           (if (compatible? context expr)
               (loop (access context expr) (pop context))
               (error "Type error detected" expr context)))
          ((accessor? expr)
           (loop (cadr expr) (push-access expr context)))
          (else
           (reconstruct context (map (lambda (arg)
                                       (loop arg (empty-context)))
                                     expr))))))


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
          (else
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

(define (sra-expression expr env)
  ;; An SRA environment is not like a normal environment.  This
  ;; environment maps every bound name to two things: the shape it had
  ;; before SRA and the list of names that have been assigned by SRA
  ;; to hold its primitive parts.  The list is parallel to the fringe
  ;; of the shape.  Note that the compound structure (vector) has an
  ;; empty list of primitive parts.
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
    (if (primitive? shape)
        1
        (reduce + 0 (map count-meaningful-parts (sra-parts shape)))))
  (define (primitive? shape)
    (memq shape '(real gensym)))
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
                       ,@(drop names-left
                               (count-meaningful-parts (car shape-left)))
                       (cdr shape-left)))))))
  (define (select-from-shape-by-access old-shape access-form)
    (cond ((eq? (car access-form) 'car)
           (cadr old-shape))
          ((eq? (car access-form) 'cdr)
           (caddr old-shape))
          ((eq? (car access-form) 'vector-ref)
           (list-ref (cdr old-shape) (caddr access-form)))))
  ;; The win continuation accepts the new, SRA'd expression, and the
  ;; shape of the value it used to return before SRA.
  (define (loop expr env win)
    (cond ((symbol? expr)
           (win `(values ,@(get-names expr env))
                (get-shape expr env)))
          ((number? expr)
           (win `(values ,expr) 'real))
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
                         (win `(let-values ,(map list new-name-sets
                                                 new-bind-expressions)
                                 ,new-body)
                              body-shape))))))
                 (error "Malformed LET" expr))))
          ((accessor? expr)
           (loop (cadr expr) env
            (lambda (new-cadr cadr-shape)
              ;; new-cadr must be a values expression
              (win (slice-values-by-access new-cadr cadr-shape expr)
                   (select-from-shape-by-access cadr-shape expr)))))
          ((construction? expr)
           (loop* (cdr expr) env
            (lambda (new-terms terms-shapes)
              ;; new-terms must all be values expressions
              (win (append-values new-terms)
                   (construct-shape terms-shapes expr)))))
          (else ;; general application
           (loop* (cdr expr) env
            (lambda (new-args args-shapes)
              ;; new-args must all be values expressions
              ;; can check type correctness of this call here
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
  (loop expr env (lambda (new-expr shape)
                   ;; Could match the shape to the externally known
                   ;; type, if desired.
                   new-expr)))

(define (lookup-return-type foo)
  'real) ;; Stub

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
;;; into the predicate position of an IF).

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

;;; The post-processor above is necessary for compatibility with MIT
;;; Scheme semantics for VALUES and primitives (namely that primitives
;;; return objects, and an object is not auto-coerced to (VALUES
;;; <object>)).  However, it requires that the forms it operates on be
;;; alpha renamed.

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
  (define (augment-env env old-names aliases)
    (append
     (map (lambda (old-name alias)
            (if (non-alias? alias)
                (cons old-name old-name)
                (cons old-name alias)))
          old-names
          (if (non-alias? aliases)
              (make-list (length old-names) the-non-alias)
              aliases))
     env))
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
                    (loop body (augment-env
                                env
                                (map car bindings)
                                (map (lambda (bind-name-list)
                                       ;; These better all be singletons
                                       (if (non-alias? bind-name-list)
                                           the-non-alias
                                           (car bind-name-list)))
                                     bind-name-lists))
                     (lambda (new-body body-name-list)
                       ;; The interior of augment-env knows which of
                       ;; these bindings are guaranteed to be dead
                       ;; because the variables being bound are
                       ;; aliases and have already been replaced in
                       ;; the new body.  I could eliminate them.
                       (win `(let ,(map list (map car bindings)
                                        new-bind-expressions)
                               ,new-body)
                            body-name-list)))))
                 (error "Malformed LET" expr))))
          ((let-values-form? expr)
           (let* ((binding (caadr expr))
                  (names (car binding))
                  (subexpr (cadr binding))
                  (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop subexpr env
                  (lambda (new-subexpr subexpr-names)
                    (loop body (augment-env env names subexpr-names)
                     (lambda (new-body body-name-list)
                       ;; The interior of augment-env knows which of
                       ;; these bindings are guaranteed to be dead
                       ;; because the variables being bound are
                       ;; aliases and have already been replaced in
                       ;; the new body.  I could eliminate them, but
                       ;; that would require traversing subexpr again
                       ;; to look for the VALUES that supplies the
                       ;; corresponding values.
                       (win `(let-values ((,names ,new-subexpr))
                               ,new-body)
                            body-name-list)))))
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
