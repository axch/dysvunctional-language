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
;;; assumed to always produce non-canonical objects, and procedure
;;; formal parameters are always assumed not to be aliases.

(define (intraprocedural-de-alias program)
  (define (de-alias-entry-point expression)
    (de-alias-expression expression (empty-alias-env)))
  (define de-alias-definition
    (rule `(define ((? name ,fol-var?) (?? formals))
             (argument-types (?? stuff))
             (? body))
          `(define (,name ,@formals)
             (argument-types ,@stuff)
             ,(de-alias-expression body (fresh-alias-env formals)))))
  (if (begin-form? program)
      (append
       (map de-alias-definition (except-last-pair program))
       (list (de-alias-entry-point (last program))))
      (de-alias-entry-point program)))

(define (de-alias-expression expr env)
  ;; An alias environment is not like a normal environment.  This
  ;; environment maps every bound name to whether it is an alias or
  ;; not; the latter case is represented by binding the variable to
  ;; itself.  It is important to know when such an environment does
  ;; not bind a variable at all; that means that variable in not in
  ;; scope here.  For purposes of this process, (constant) numbers,
  ;; booleans, and empty-lists are legitimate things that variables
  ;; may be aliases of (and are always in scope).
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
    (cond ((fol-var? expr)
           (de-alias-fol-var expr env win))
          ((number? expr)
           (win expr (list expr)))
          ((boolean? expr)
           (win expr (list expr)))
          ((null? expr)
           (win expr (list expr)))
          ((if-form? expr)
           (de-alias-if expr env win))
          ((let-form? expr)
           (de-alias-let expr env win))
          ((let-values-form? expr)
           (de-alias-let-values expr env win))
          ((values-form? expr)
           (de-alias-values expr env win))
          (else ; general application
           (de-alias-application expr env win))))
  (define (de-alias-fol-var expr env win)
    (let ((alias-binding (find-alias expr env)))
      (if alias-binding
          (win (cdr alias-binding) (list (cdr alias-binding)))
          (error "Trying to de-alias an unbound variable" expr env))))
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
      (loop* (map cadr bindings) env
       (lambda (new-bind-expressions bind-name-lists)
         (let ((bind-names
                (map (lambda (bind-name-list)
                       ;; These better all be singletons
                       (if (non-alias? bind-name-list)
                           the-non-alias
                           (car bind-name-list)))
                     bind-name-lists)))
           (augment-alias-env env (map car bindings) bind-names
            (lambda (env acceptable-aliases)
              (loop body env
               (lambda (new-body body-name-list)
                 (degment-alias-env! env (map car bindings))
                 (win (empty-let-rule
                       `(let ,(filter-map
                               (lambda (name alias? expr)
                                 (and (not alias?)
                                      (list name expr)))
                               (map car bindings)
                               acceptable-aliases
                               new-bind-expressions)
                          ,new-body))
                      body-name-list))))))))))
  (define (de-alias-let-values expr env win)
    (let* ((binding (caadr expr))
           (names (car binding))
           (subexpr (cadr binding))
           (body (caddr expr)))
      (loop subexpr env
       (lambda (new-subexpr subexpr-names)
         (augment-alias-env env names subexpr-names
          (lambda (env acceptable-aliases)
            (loop body env
             (lambda (new-body body-name-list)
               ;; ACCEPTABLE-ALIASES tells me which of these bindings
               ;; are guaranteed to be dead because the variables
               ;; being bound are aliases and have already been
               ;; replaced in the new body.  I could eliminate them,
               ;; but that would require traversing subexpr again to
               ;; look for the VALUES that supplies the corresponding
               ;; values.  For now, I will just kill the whole
               ;; let-values if it is useless.
               (degment-alias-env! env names)
               (win (if (any not acceptable-aliases)
                        `(let-values ((,names ,new-subexpr))
                           ,new-body)
                        new-body)
                    body-name-list)))))))))
  (define (de-alias-values expr env win)
    (loop* (cdr expr) env
     (lambda (exprs names-lists)
       (win `(values ,@exprs) (apply append names-lists)))))
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

(define (empty-alias-env)
  (make-eq-hash-table))

(define (fresh-alias-env names)
  (augment-alias-env (empty-alias-env) names names
   (lambda (env acceptable-aliases)
     env)))

(define (augment-alias-env env old-names aliases win)
  (define (acceptable-alias? alias)
    (and (not (non-alias? alias))
         (or (number? alias)
             (boolean? alias)
             (null? alias)
             (find-alias alias env))))
  (let ((aliases (if (non-alias? aliases)
                     (make-list (length old-names) the-non-alias)
                     aliases)))
    (for-each (lambda (old-name alias)
                (if (acceptable-alias? alias)
                    (hash-table/put! env old-name alias)
                    (hash-table/put! env old-name old-name)))
              old-names
              aliases)
    (win env (map acceptable-alias? aliases))))

(define (degment-alias-env! env names)
  (for-each (lambda (name)
              (hash-table/remove! env name))
            names))

(define (find-alias name env)
  (hash-table/lookup env name
    (lambda (datum)
      (cons name datum))
    (lambda () #f)))

(define the-non-alias (list 'not-an-alias))

(define (non-alias? thing)
  (eq? the-non-alias thing))

;;; Must-alias analysis is a subset of common subexpression
;;; elimination.  The de-aliasing recursion can be extended to being
;;; full common subexpression elimination as follows.  Replace the
;;; concept of "non-canonical object" with "symbolic expression of
;;; other canonical objects".  Bring down a table of which symbolic
;;; expressions have which canonical names.  Return up, along with the
;;; rewritten subexpression, the minimal symbolic expression that
;;; describes what this expression returns in terms of existing
;;; in-scope canonical objects.  At procedure calls: cons up the
;;; symbolic expression for what that procedure does to its arguments.
;;; If that symbolic expression already exists in the table, the call
;;; can be replaced with a reference to that canonical name, and that
;;; canonical name can be returned.  If not, return that expression.
;;; Impure procedures should always synthesize new symbolic answers,
;;; that won't register as existing in the table (but may if they are
;;; then themselves aliased).  If a LET expression returns a single
;;; in-scope, then the bound variable has that name as its canonical
;;; name.  If not, the bound variable becomes the new canonical name
;;; for that expression.  IFs are not different from procedure calls
;;; (except that if the consequent and alternate are the same
;;; expression, then that's the result of the whole IF).  Accesses
;;; invert the canonical expressions that are the corresponding
;;; constructions in the appropriate way.  This whole thing will work
;;; a whole lot better if the input is in A-normal form, because
;;; intermediate values will always get names, and there will be a
;;; maximum of opportunities for detecting commonalities.  It will
;;; probably also work better if all lets are lifted, because then
;;; previously computed subexpressions will spend more time in scope.
;;; In the place where I said "minimal symbolic expression", there is
;;; ample room for algebraic simplification, if desired.

(define (intraprocedural-cse program)
  (define (cse-entry-point expression)
    (cse-expression expression (empty-cse-env)))
  (define cse-definition
    (rule `(define ((? name ,fol-var?) (?? formals))
             (argument-types (?? stuff))
             (? body))
          `(define (,name ,@formals)
             (argument-types ,@stuff)
             ,(cse-expression body (fresh-cse-env formals)))))
  (if (begin-form? program)
      (append
       (map cse-definition (except-last-pair program))
       (list (cse-entry-point (last program))))
      (cse-entry-point program)))

(define (cse-expression expr env)
  ;; A CSE environment is not like a normal environment.  This
  ;; environment maps every symbolic expression to the name of the
  ;; canonical variable that holds that expression, if any (notes: an
  ;; expression whose value is a known constant will be mapped to that
  ;; constant; a variable may be mapped to itself to indicate that it
  ;; holds something that no previously computed variable holds).  It
  ;; is important to know when such an environment does not bind a
  ;; variable at all; that means that variable in not in scope here
  ;; (constants are always in scope).
  (define (merge-name-lists names1 names2) ; <-- symbolic-if
    (if (or (non-alias? names1) (non-alias? names2))
        the-non-alias
        (map (lambda (name1 name2)
               (if (eq? name1 name2)
                   name1
                   the-non-alias))
             names1 names2)))
  ;; This is written in continuation passing style because recursive
  ;; calls must return two things.  The win continuation accepts the
  ;; new, CSE'd expression, the symbolic expression representing the
  ;; return value from this expression (the symbolic expressions for
  ;; the elements of a multivalue return are held in a parallel list).
  (define (loop expr env win)
    (cond ((fol-var? expr)
           (cse-fol-var expr env win))
          ((number? expr)
           (win expr expr))
          ((boolean? expr)
           (win expr expr))
          ((null? expr)
           (win expr expr))
          ((if-form? expr)
           (cse-if expr env win))
          ((let-form? expr)
           (cse-let expr env win))
          ((let-values-form? expr)
           (cse-let-values expr env win))
          ((values-form? expr)
           (cse-values expr env win))
          (else ; general application
           (cse-application expr env win))))
  (define (cse-fol-var expr env win)
    (let ((alias-binding (find-alias expr env)))
      (if alias-binding
          (win (cdr alias-binding) (cdr alias-binding))
          (error "Trying to cse an unbound variable" expr env))))
  (define (cse-if expr env win)
    (loop (cadr expr) env
     (lambda (new-pred symbolic-pred)
       (loop (caddr expr) env
        (lambda (new-cons symbolic-cons)
          (loop (cadddr expr) env
           (lambda (new-alt symbolic-alt)
             (win `(if ,new-pred ,new-cons ,new-alt)
                  (symbolic-if symbolic-pred symbolic-cons symbolic-alt)))))))))
  (define (cse-let expr env win)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (loop* (map cadr bindings) env
       (lambda (new-bind-expressions bound-symbolics)
         (augment-cse-env env (map car bindings) bound-symbolics
          (lambda (env dead-bindings)
            (loop body env
             (lambda (new-body body-symbolic)
               (degment-cse-env! env (map car bindings))
               (win (empty-let-rule
                     `(let ,(filter-map
                             (lambda (name dead? expr)
                               (and (not dead?)
                                    (list name expr)))
                             (map car bindings)
                             dead-bindings
                             new-bind-expressions)
                        ,new-body))
                    body-symbolic)))))))))
  (define (cse-let-values expr env win)
    (let* ((binding (caadr expr))
           (names (car binding))
           (subexpr (cadr binding))
           (body (caddr expr)))
      (loop subexpr env
       (lambda (new-subexpr subexpr-symbolic)
         (augment-cse-env env names subexpr-symbolic
          (lambda (env dead-bindings)
            (loop body env
             (lambda (new-body body-symbolic)
               ;; DEAD-BINDINGS tells me which of these bindings
               ;; are guaranteed to be dead because the variables
               ;; being bound are aliases and have already been
               ;; replaced in the new body.  I could eliminate them,
               ;; but that would require traversing subexpr again to
               ;; look for the VALUES that supplies the corresponding
               ;; values.  For now, I will just kill the whole
               ;; let-values if it is useless.
               (degment-cse-env! env names)
               (win (if (any not dead-bindings)
                        `(let-values ((,names ,new-subexpr))
                           ,new-body)
                        new-body)
                    body-symbolic)))))))))
  (define (cse-values expr env win)
    (loop* (cdr expr) env
     (lambda (exprs symbolics)
       (win `(values ,@exprs) symbolics))))
  (define (cse-application expr env win)
    (loop* (cdr expr) env
     (lambda (new-args args-symbolics)
       (let* ((symb-candidate (symbolic-application (car expr) args-symbolics))
              (symbolic (cse-canonical env symb-candidate)))
         (win (if (or (fol-var? symbolic) (fol-const? symbolic))
                  symbolic
                  `(,(car expr) ,@new-args))
              symbolic)))))
  (define (loop* exprs env win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) env
         (lambda (new-expr expr-symbolic)
           (loop* (cdr exprs) env
            (lambda (new-exprs expr-symbolics)
              (win (cons new-expr new-exprs)
                   (cons expr-symbolic expr-symbolics))))))))
  (loop expr env (lambda (new-expr symbolic)
                   ;; The symbolic expression might be useful to an
                   ;; interprocedural CSE crunch.
                   new-expr)))

(define (fol-const? thing)
  (or (number? thing) (boolean? thing) (null? thing)))

(define (symbolic-application operator arguments)
  (define simplify
    ;; There are lots of possibilities for simplification here,
    ;; especially if I request the environment and look up the
    ;; symbolic expressions that various variables among the arguments
    ;; hold.
    (rule-simplifier
     (list
      (rule `(+ 0 (? thing)) thing)
      (rule `(+ (? thing) 0) thing)
      (rule `(- (? thing) 0) thing)

      (rule `(* 1 (? thing)) thing)
      (rule `(* (? thing) 1) thing)
      (rule `(/ (? thing) 1) thing))))
  (simplify
   (cond ((memq operator '(read-real real gensym))
          unique-expression)
         ;; Somewhere around here I also have a choice as to whether
         ;; this CSE will have the effect of identifying equal pairs.
         (else `(,operator ,@arguments)))))

(define (symbolic-if pred cons alt)
  (if (equal? cons alt)
      cons
      `(if ,pred ,cons ,alt)))

(define-structure unique-expression)
(define unique-expression (make-unique-expression))

(define (cse-canonical env symbolic)
  (hash-table/get env symbolic symbolic))

(define (empty-cse-env)
  (make-equal-hash-table))

(define (fresh-cse-env names)
  (augment-cse-env (empty-cse-env) names names
   (lambda (env dead-bindings)
     env)))

(define (augment-cse-env env names symbolics win)
  (define (acceptable-alias? symbolic)
    (or (fol-const? symbolic)
        (and (fol-var? symbolic)
             (find-alias symbolic env))))
  (for-each (lambda (name symbolic)
              ;; Canonizing here catches places where a parallel let
              ;; binds the same expression to multiple names.
              (let ((symbolic (cse-canonical env symbolic)))
                (if (acceptable-alias? symbolic)
                    (hash-table/put! env name symbolic)
                    (begin
                      (hash-table/put! env name name)
                      ;; Don't put variables back because if a
                      ;; variable is not acceptable alias, then it's
                      ;; out of scope, and putting it back would
                      ;; forget that.  Also don't put unique
                      ;; expressions in at all, because they represent
                      ;; situations where repeating the same
                      ;; expression (e.g. (read-real)) would have
                      ;; different effects.
                      (if (and (not (fol-var? symbolic))
                               (not (unique-expression? symbolic)))
                          (hash-table/put! env symbolic name))))))
            names
            symbolics)
  (win env (map acceptable-alias? symbolics)))

(define (degment-cse-env! env names)
  (for-each (lambda (name)
              (hash-table/remove! env name))
            names))
