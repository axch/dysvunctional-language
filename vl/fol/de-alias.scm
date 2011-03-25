(declare (usual-integrations))
;;;; Alias elimination

;;; Alias elimination is the process of removing variables that can be
;;; statically determined to always contain the same values as other
;;; variables, or as constants.  This can also be called constant
;;; propagation and copy propagation.  Alias elimination is very
;;; important in compiling FOL, because between them the A-normal form
;;; conversion and the subsequent scalar replacement of aggregates
;;; introduce a huge number of variables, many of which end up being
;;; aliases.

;;; I only do intraprocedural alias elimination.  Interprocedural
;;; alias elimination is substantially hairier.  I will probably need
;;; it eventually, but I can do without for now.

;;; Intraprocedural alias elimination is a structural recursion over
;;; the code, walking LET bindings before LET bodies.  The recursion
;;; carries down an environment mapping every bound name to the
;;; canonical object of which it is an alias (itself, in the case of
;;; non-aliases).  This environment is also used to detect when some
;;; name is not in scope somewhere.  The recursive call returns the
;;; de-aliased subexpression and also the canonical object of which
;;; the return value of the subexpression is an alias, or a sentinel
;;; if the subexpression does not return such an object (for example,
;;; procedure calls or IF statements with sufficiently different
;;; branches).  The canonical object may be a list to handle multiple
;;; value returns and corresponding parallel bindings.

;;; In this setup, a constant becomes itself and is its own canonical
;;; object.  A variable reference becomes and forwards its canonical
;;; object (which may be itself if the variable is not an alias).
;;; Multiple value returns aggregate lists of canonical objects.  IF
;;; forms intersect the canonical object lists of their branches: if
;;; different branches may return different things in the same slot,
;;; that slot is marked as not canonical (meaning whatever value it
;;; will be bound to is not an alias).  LET forms process their
;;; binding form first.  If the binding form is a canonical object
;;; that is in scope in the whole LET expression, then the bound
;;; variable is an alias to that object and can be so marked;
;;; otherwise the variable is not an alias.  This being determined,
;;; the body of the LET can be transformed.  LET-VALUES are analagous,
;;; but take parallel lists of canonical objects.  Because this is an
;;; intraprocedural alias elimination, procedure applications are
;;; assume to always produce non-canonical objects, and procedure
;;; formal parameters are always assumed not to be aliases.

(define (intraprocedural-de-alias program)
  (define de-alias-definition
    (rule `(define ((? name ,symbol?) (?? formals))
             (argument-types (?? stuff))
             (? body))
          `(define (,name ,@formals)
             (argument-types ,@stuff)
             ,(de-alias-expression body (map cons formals formals)))))
  (if (begin-form? program)
      (append
       (map de-alias-definition (except-last-pair program))
       (list (de-alias-expression (last program) '())))
      (de-alias-expression program '())))

(define (de-alias-expression expr env)
  ;; An alias environment is not like a normal environment.  This
  ;; environment maps every bound name to whether it is an alias or
  ;; not; the latter case is represented by binding the variable to
  ;; itself.  It is important to know when such an environment does
  ;; not bind a variable at all; that means that variable in not in
  ;; scope here.  For purposes of this process, (constant) numbers,
  ;; booleans, and empty-lists are legitimate things that variables
  ;; may be aliases of (and are always in scope).
  (define (augment-env env old-names aliases win)
    (define (acceptable-alias? alias)
      (and (not (non-alias? alias))
           (or (number? alias)
               (boolean? alias)
               (null? alias)
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
  ;; This is written in continuation passing style because recursive
  ;; calls must return two things.  The win continuation accepts the
  ;; new, de-aliased expression, and a list of the names of the
  ;; variables that hold the return values from this expression.
  (define (loop expr env win)
    (cond ((symbol? expr)
           (de-alias-symbol expr env win))
          ((number? expr)
           (win expr (list expr)))
          ((boolean? expr)
           (win expr (list expr)))
          ((null? expr)
           (win expr (list expr)))
          ((values-form? expr)
           (de-alias-values expr env win))
          ((if-form? expr)
           (de-alias-if expr env win))
          ((let-form? expr)
           (de-alias-let expr env win))
          ((let-values-form? expr)
           (de-alias-let-values expr env win))
          (else ; general application
           (de-alias-application expr env win))))
  (define (de-alias-symbol expr env win)
    (let ((alias-binding (lookup expr env)))
      (if alias-binding
          (win (cdr alias-binding) (list (cdr alias-binding)))
          (error "Trying to de-alias an unbound variable" expr env))))
  (define (de-alias-values expr env win)
    (loop* (cdr expr) env
     (lambda (exprs names-lists)
       (win `(values ,@exprs) (apply append names-lists)))))
  (define (de-alias-if expr env win)
    (loop (cadr expr) env
     (lambda (new-pred pred-names)
       (loop (caddr expr) env
        (lambda (new-cons cons-names)
          (loop (cadddr expr) env
           (lambda (new-alt alt-names)
             (win `(if ,new-pred ,new-cons ,new-alt)
                  (merge-name-lists cons-names alt-names)))))))))
  (define (de-alias-let expr env win)
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
  (define (de-alias-let-values expr env win)
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
  (define (de-alias-application expr env win)
    (loop* (cdr expr) env
     (lambda (new-args args-names-lists)
       (win `(,(car expr) ,@new-args)
            the-non-alias))))
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
