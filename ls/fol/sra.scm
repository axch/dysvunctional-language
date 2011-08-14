(declare (usual-integrations))
;;;; Scalar Replacement of Aggregates (SRA)

;;; Imagine a program like this:
;;; (let ((pair (cons foo bar)))
;;;   ... (car pair) ... (cdr pair)) ; pair does not escape
;;; If something like this gets interpreted or compiled naively, it
;;; will heap-allocate a cons cell, and then eventually spend time
;;; garbage collecting it.  We could, however, have just put FOO and
;;; BAR into local variables instead.  This is the objective of SRA.

;;; SRA replaces, whenever possible, the construction of data
;;; structures with just returning their pieces with VALUES, and the
;;; receipt of said data structures with catching said VALUES with
;;; LET-VALUES.  If the program being SRA'd is union-free, this will
;;; always be possible, until no data structures are left (except any
;;; that are built just to be given to the outside world).

;;; This implementation of SRA uses type information about the
;;; procedures in FOL to transform both their definitions and their
;;; call sites in synchrony.  The definition of a procedure is changed
;;; to accept the pieces of input data structures as separate
;;; arguments and to return the pieces of the output data structure as
;;; a multivalue return.  Calls to a procedure are changed to supply
;;; the pieces of input data structures as separate arguments.
;;; Procedure bodies are transformed by recursive descent to match.

;;; The structure of the recursive descent is as follows: walk down
;;; the expression (traversing LET bindings before LET bodies)
;;; carrying a map from all bound variables to a) the shape that
;;; variable used to have before SRA, and b) the set of names that
;;; have been assigned to hold its useful pieces.  Return, from each
;;; subexpression, the SRA'd subexpression together with the shape of
;;; the value that subexpression used to return before SRA.  When a
;;; shape being returned hits a binding site, invent names for the
;;; useful pieces of the shape, and change the binding site to bind
;;; those names instead of the original shape.

;;; When union types are added to FOL, the effect will be the addition
;;; of types that are "primitive" as far as the SRA process in
;;; concerned, while being "compound" in the actual underlying code.
;;; Which types to mark primitive and which to expand out will be
;;; decided by finding a feedback vertex set in the type reference
;;; graph.

(define (scalar-replace-aggregates program)
  (%scalar-replace-aggregates (approximate-anf program)))

(define (scalar-replace-aggregates-visibly program)
  ((visible-stage %scalar-replace-aggregates)
   ((visible-stage approximate-anf)
    program)))

;;; The actual implementation of SRA makes its bookkeeping easier by
;;; first converting its input to approximate A-normal form with
;;; APPROXIMATE-ANF (see a-normal-form.scm).  As it happens, the
;;; output remains in approximate A-normal form (TODO except for the
;;; influence of RECONSTRUCT-PRE-SRA-SHAPE), but no other stage cares
;;; about this.

;;; The FOL grammar accepted by the SRA algorithm replaces the
;;; standard FOL <expression>, <access>, and <construction> with
;;;
;;; expression = <simple-expression>
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)
;;;            | (let-values (((<data-var> <data-var> <data-var> ...) <expression>))
;;;                <expression>)
;;;            | <access>
;;;            | <construction>
;;;            | (values <simple-expression> <simple-expression> <simple-expression> ...)
;;;            | (<proc-var> <simple-expression> ...)
;;;
;;; simple-expression = <data-var> | <number> | <boolean> | ()
;;;
;;; access = (car <simple-expression>)
;;;        | (cdr <simple-expression>)
;;;        | (vector-ref <simple-expression> <integer>)
;;;
;;; construction = (cons <simple-expression> <simple-expression>)
;;;              | (vector <simple-expression> ...)
;;;
;;; This is the grammar of the output of APPROXIMATE-ANF.

;;; Given ANF, a construction (CONS or VECTOR form) becomes a
;;; multivalue return of the appended names for the things being
;;; constructed (which I have, because of the ANF); an access becomes
;;; a multivalue return of an appropriate slice of the names being
;;; accessed (which I again have because of the ANF); a call becomes
;;; applied to the append of the names for each former element in the
;;; call (ANF strikes again); a name becomes a multivalue return of
;;; those assigned names; a constant remains a constant; a let
;;; becomes a multi-value let (whereupon I invent the names to hold
;;; the pieces of the that used to be bound here); and a definition
;;; becomes a definition taking the appropriately larger number of
;;; arguments, whose internal names I can invent at this point.  The
;;; entry point is transformed without any initial name bindings, and
;;; care is taken to reconstruct the shape the outside world expects
;;; (see RECONSTRUCT-PRE-SRA-SHAPE).

(define (%scalar-replace-aggregates program)
  (let ((lookup-type (type-map program)))
    ;; TODO This is not idempotent because if repeated on an entry
    ;; point that has to produce a structure, it will re-SRA the
    ;; reconstruction code and then add more reconstruction code to
    ;; reconstruct the output of that.  Perhaps this can be fixed.
    (define (sra-entry-point expression)
      (sra-expression
       expression (empty-env) lookup-type reconstruct-pre-sra-shape))
    (define sra-definition
      (rule `(define ((? name ,fol-var?) (?? formals))
               (argument-types (?? arg-shapes) (? return))
               (? body))
            (let* ((new-name-sets
                    (map invent-names-for-parts formals arg-shapes))
                   (env (augment-env
                         (empty-env) formals new-name-sets arg-shapes))
                   (new-names (apply append new-name-sets)))
              `(define (,name ,@new-names)
                 (argument-types
                  ,@(append-map primitive-fringe arg-shapes)
                  ,(tidy-values `(values ,@(primitive-fringe return))))
                 ,(sra-expression body env lookup-type
                                  (lambda (new-body shape) new-body))))))
    (if (begin-form? program)
        (append
         (map sra-definition (except-last-pair program))
         (list (sra-entry-point (last program))))
        (sra-entry-point program))))

(define (sra-expression expr env lookup-type win)
  ;; An SRA environment maps every bound name to two things: the shape
  ;; it had before SRA and the list of names that have been assigned
  ;; by SRA to hold its primitive parts.  The list is parallel to the
  ;; fringe of the shape.  Note that the compound structure (vector)
  ;; has an empty list of primitive parts.
  ;; This is written in continuation passing style because I need two
  ;; pieces of information from the recursive call.  The win
  ;; continuation accepts the new, SRA'd expression, and the shape of
  ;; the value it used to return before SRA.
  ;; For this purpose, a VALUES is the same as any other construction.
  (define (construction? expr)
    (and (pair? expr)
         (memq (car expr) '(cons vector values))))
  (define (loop expr env win)
    (cond ((fol-var? expr)
           (win (tidy-values `(values ,@(get-names expr env)))
                (get-shape expr env)))
          ((number? expr)
           (win expr 'real))
          ((boolean? expr)
           (win expr 'bool))
          ((null? expr)
           (win `(values) '()))
          ((if-form? expr)
           (sra-if expr env win))
          ((let-form? expr)
           (sra-let expr env win))
          ((let-values-form? expr)
           (sra-let-values expr env win))
          ((lambda-form? expr)
           (sra-lambda expr env win))
          ((accessor? expr)
           (sra-access expr env win))
          ((construction? expr)
           (sra-construction expr env win))
          (else ;; general application
           (sra-application expr env win))))
  (define (sra-if expr env win)
    (loop (cadr expr) env
     (lambda (new-pred pred-shape)
       (assert (eq? 'bool pred-shape))
       (loop (caddr expr) env
        (lambda (new-cons cons-shape)
          (loop (cadddr expr) env
           (lambda (new-alt alt-shape)
             ;; TODO cons-shape and alt-shape better be the same
             ;; (or at least compatible)
             (win `(if ,new-pred ,new-cons ,new-alt)
                  cons-shape))))))))
  (define (sra-let expr env win)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (loop* (map cadr bindings) env
       (lambda (new-bind-expressions bind-shapes)
         (let ((new-name-sets
                (map invent-names-for-parts
                     (map car bindings) bind-shapes)))
           (loop body (augment-env
                       env (map car bindings)
                       new-name-sets bind-shapes)
            (lambda (new-body body-shape)
              (win (if (every (lambda (set)
                                (= 1 (length set)))
                              new-name-sets)
                       ;; Opportunistically preserve parallel LET with
                       ;; non-structured expressions.
                       `(let ,(map list (map car new-name-sets)
                                   new-bind-expressions)
                          ,new-body)
                       (tidy-let-values
                        `(let-values ,(map list new-name-sets
                                           new-bind-expressions)
                           ,new-body)))
                   body-shape))))))))
  (define (sra-let-values expr env win)
    (let* ((binding (caadr expr))
           (body (caddr expr))
           (names (car binding))
           (exp (cadr binding)))
      (loop exp env
       (lambda (new-exp exp-shape)
         (let ((new-name-sets
                (map invent-names-for-parts names (sra-parts exp-shape))))
           ;; The previous line is not idempotent because it renames
           ;; all the bindings, even those that used to hold
           ;; non-structured values.
           (loop body (augment-env env names new-name-sets (sra-parts exp-shape))
            (lambda (new-body body-shape)
              (win (tidy-let-values
                    `(let-values ((,(apply append new-name-sets) ,new-exp))
                       ,new-body))
                   body-shape))))))))
  (define (sra-lambda expr env win)
    (let ((formal (cadr expr))
          (body (caddr expr)))
      ;; TODO Generalize to arg types other than real
      (loop body (augment-env env formal (list formal) (list 'real))
       (lambda (new-body body-shape)
         (win `(lambda ,formal
                 ,(reconstruct-pre-sra-shape new-body body-shape))
              (function-type 'real body-shape))))))
  (define (sra-access expr env win)
    (loop (cadr expr) env
     (lambda (new-cadr cadr-shape)
       (win (slice-values-by-access new-cadr cadr-shape expr)
            (select-from-shape-by-access cadr-shape expr)))))
  (define (sra-construction expr env win)
    (loop* (cdr expr) env
     (lambda (new-terms terms-shapes)
       (win (append-values new-terms)
            (construct-shape terms-shapes expr)))))
  (define (sra-application expr env win)
    (loop* (cdr expr) env
     (lambda (new-args args-shapes)
       ;; The type checker should have ensured this
       ;(assert (every equal? args-shapes (arg-types (lookup-type (car expr))))
       (win `(,(car expr) ,@(smart-values-subforms (append-values new-args)))
            (return-type (lookup-type (car expr)))))))
  (define (loop* exprs env win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) env
         (lambda (new-expr expr-shape)
           (loop* (cdr exprs) env
            (lambda (new-exprs expr-shapes)
              (win (cons new-expr new-exprs)
                   (cons expr-shape expr-shapes))))))))
  (loop expr env (lambda (new-expr shape) (win new-expr shape))))

;;; The following post-processor is necessary for compatibility with
;;; MIT Scheme semantics for multiple value returns (namely that a
;;; unary multiple value return is distinguished in MIT Scheme from an
;;; ordinary return).  This post-processor also ensures that
;;; LET-VALUES forms only bind the results of one expression; this
;;; requires that the forms it operates on be alpha renamed.

(define trivial-let-values-rule
  (rule `(let-values (((? names) (values (?? stuff))))
           (?? body))
        `(let ,(map list names stuff)
           ,@body)))

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
                         (((? name ,fol-var?)) (? exp)))
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
              ,(trivial-let-values-rule
                `(let-values (,binding2)
                   ,@body))))
     trivial-let-values-rule))))

;;; Reconstruction of the shape the outside world expects.

;;; The NEW-EXPR argument is an expression (freshly produced by SRA)
;;; that returns some contentful stuff as a multiple value return, and
;;; the SHAPE argument is the shape this stuff used to have before
;;; SRA, and which, presumably, the outside world still expects.  The
;;; expression to produce that consists, in the general case, of
;;; multiple value binding the incoming pieces to synthetic names, and
;;; reconstructing the desired structure out of those variables.  The
;;; actual reconstructing expression is built by structural recursion
;;; on the shape, cdring down the list of synthetic names in parallel
;;; with the fringe of the shape.

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

;;; SRA supporting procedures

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
  (or (function-type? shape)
      (memq shape '(real bool gensym escaping-function))))
(define (primitive-fringe shape)
  (cond ((null? shape) '())
        ((primitive-shape? shape) (list shape))
        (else (append-map primitive-fringe (sra-parts shape)))))
(define (sra-parts shape)
  (cond ((construction? shape)
         (cdr shape))
        ((values-form? shape)
         (cdr shape))
        (else (list shape))))
(define (invent-names-for-parts basename shape)
  (let ((count (count-meaningful-parts shape)))
    (if (= 1 count)
        (list basename)
        (map (lambda (i) (make-name basename))
             (iota count)))))
(define (smart-values-subforms form)
  (if (values-form? form)
      (cdr form)
      (list form)))
(define (append-values values-forms)
  (tidy-values `(values ,@(append-map smart-values-subforms values-forms))))
(define (construct-shape subshapes template)
  `(,(car template) ,@subshapes))
(define (slice-values-by-access values-form old-shape access-form)
  (let ((the-subforms (smart-values-subforms values-form)))
    (tidy-values
     (cond ((eq? (car access-form) 'car)
            `(values ,@(take the-subforms
                             (count-meaningful-parts (cadr old-shape)))))
           ((eq? (car access-form) 'cdr)
            `(values ,@(drop the-subforms
                             (count-meaningful-parts (cadr old-shape)))))
           ((eq? (car access-form) 'vector-ref)
            (let loop ((index-left (caddr access-form))
                       (names-left the-subforms)
                       (shape-left (cdr old-shape)))
              (if (= 0 index-left)
                  `(values ,@(take names-left
                                   (count-meaningful-parts (car shape-left))))
                  (loop (- index-left 1)
                        (drop names-left
                              (count-meaningful-parts (car shape-left)))
                        (cdr shape-left)))))))))
(define (select-from-shape-by-access old-shape access-form)
  (cond ((eq? (car access-form) 'car)
         (cadr old-shape))
        ((eq? (car access-form) 'cdr)
         (caddr old-shape))
        ((eq? (car access-form) 'vector-ref)
         (list-ref (cdr old-shape) (caddr access-form)))))
