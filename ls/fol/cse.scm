(declare (usual-integrations))
;;;; Common subexpression elimination

;;; Common subexpression elimination (CSE) is the process of removing
;;; expressions that can be statically determined to always return the
;;; same values as are already stored in in-scope variables (where a
;;; constant stores itself and is always in scope).  This includes
;;; constant propagation and copy propagation as special cases with
;;; trivial common subexpressions.  Common subexpression elimination
;;; is very important in compiling FOL, because between them the
;;; A-normal form conversion and the subsequent scalar replacement of
;;; aggregates introduce a huge number of variables, many of which end
;;; up being just aliases of other variables, and thus removable.
;;; Also, the interaction of the flow analysis making multiple
;;; variants of a procedure and the inliner inlining them can generate
;;; more interesting common subexpressions.

;;; I only do intraprocedural CSE.  Interprocedural CSE is
;;; substantially hairier.  I will probably need it eventually (to
;;; eliminate interprocedural aliases, if nothing else), but I can do
;;; without for now.

;;; Intraprocedural CSE is a structural recursion over the code,
;;; walking LET bindings before LET bodies.  The recursion carries
;;; down an environment mapping every computed expression to the
;;; canonical variable storing the result of that expression.  Special
;;; circumstances: a variable mapped to itself is the canonical name
;;; for whatever object it holds; a variable mapped to something else
;;; is an alias; all variables that are keys are in scope; if an
;;; expression does not occur as a key, then it is fresh, in the sense
;;; that no in-scope variable already stores the value it would
;;; compute.  The recursive call returns the CSE'd subexpression and
;;; also the (canonized) expression said subexpression computes (there
;;; is ample potential for algebraic simplification here, but beware
;;; floating point).  This may also be a sentinel meaning "this
;;; expression is not pure, so do not eliminate copies of it".  The
;;; expression may be a values-form to handle multiple value returns
;;; and corresponding parallel bindings.

;;; In this setup:
;;; - A constant becomes itself and is its own canonical variable.
;;; - A variable reference becomes and forwards its canonical
;;;   variable (which may be itself if the variable is not an alias).
;;; - Multiple value returns aggregate lists of canonized expressions.
;;; - LET forms process their binding form first.  If the binding
;;;   form's expression is a variable or constant that is in scope in
;;;   the whole LET expression, then the bound variable is an alias to
;;;   that object and can be so marked; otherwise the variable is not
;;;   an alias, but becomes the canonical variable for itself and for
;;;   whatever expression the body computed (unless that computation
;;;   was impure).  This being determined, the body of the LET can be
;;;   transformed.
;;; - LET-VALUES are analagous, but take parallel lists of expressions
;;;   (except in the case where a procedure boundary intervenes
;;;   between the VALUES and the LET-VALUES).
;;; - Procedure applications are the places where new expressions get
;;;   constructed.  If the expression describing the value this
;;;   procedure computes from its arguments is stored in an in-scope
;;;   canonical variable, then this procedure call can be replaced by
;;;   a reference to that variable.  If not, the call is left as is
;;;   and the expression is returned (presumably some variable will
;;;   soon become the canonical variable holding this expression, when
;;;   the value returned by the procedure is bound).
;;; - IF forms are analagous to procedures, with the simplification
;;;   being that if both branches are equal then the test can be
;;;   elided (IFs also map into multivalue returns when possible).
;;; - Accesses are also analagous to procedures, with the
;;;   simplification being to invert the canonical expressions that
;;;   are the corresponding constructions in the appropriate way.
;;;   This is not implemented yet, but doesn't much matter if CSE
;;;   follows SRA.

;;; Because this is an intraprocedural common subexpression
;;; elimination, user procedures are not studied, but always assumed
;;; to be impure; and incoming procedure formal parameters are always
;;; assumed to be their own canonical variables (in particular, not
;;; aliases of each other).  This whole thing will work a whole lot
;;; better if the input is in A-normal form, because intermediate
;;; values will always get names, and there will be a maximum of
;;; opportunities for detecting commonalities.  It will probably also
;;; work better if all lets are lifted, because then previously
;;; computed subexpressions will spend more time in scope.

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
  ;;
  ;; This is written in continuation passing style because recursive
  ;; calls must return two things.  The win continuation accepts the
  ;; new, CSE'd expression, the symbolic expression representing the
  ;; return value from this expression (using a values form for the
  ;; symbolic expressions for the elements of a multivalue return).
  (define (loop expr env win)
    (cond ((fol-var? expr)
           (cse-fol-var expr env win))
          ((fol-const? expr)
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
             ;; TODO SYMBOLIC-IF detects cases where both branches of
             ;; the IF are equal.  Do I want to simplify the returned
             ;; IF expression in that case?
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
         (augment-cse-env env names
          (if (values-form? subexpr-symbolic)
              (cdr subexpr-symbolic)
              ;; If the symbolic expression is not a values form, it
              ;; must be a procedure call that returns multiple
              ;; values.  I can still represent the operation of
              ;; destructuring it, on the off chance that the same
              ;; procedure may be called again.
              (map (lambda (i)
                     `(values-ref ,subexpr-symbolic ,i))
                   (iota (length names))))
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
       (win `(values ,@exprs) `(values ,@symbolics)))))
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
  (define (user-procedure? operator)
    (not (memq operator (map primitive-name *primitives*))))
  (simplify
   (cond ((memq operator
                (cons 'real ; REAL is "statically" but not "dynamically" impure
                      (map primitive-name
                           (filter primitive-impure? *primitives*))))
          unique-expression)
         ((user-procedure? operator)    ; TODO This is painful.
          unique-expression)
         ;; Somewhere around here I also have a choice as to whether
         ;; this CSE will have the effect of identifying equal pairs.
         (else `(,operator ,@arguments)))))

(define (symbolic-if pred cons alt)
  (if (equal? cons alt)
      ;; This is somewhat dangerous, if evaluating the predicate might
      ;; have had some interesting effect like signaling a divide by
      ;; zero error.
      cons
      (if (and (values-form? cons)
               (values-form? alt))
          `(values ,@(map (lambda (sub-cons sub-alt)
                            (symbolic-if pred sub-cons sub-alt))
                          (cdr cons) (cdr alt)))
          `(if ,pred ,cons ,alt))))

(define-structure (unique-expression safe-accessors))
(define unique-expression (make-unique-expression))

(define (cse-canonical env symbolic)
  ;; Either here or in degment-cse-env!, I have to flush expressions
  ;; whose canonical variables are not in scope any more.
  (let ((candidate (hash-table/get env symbolic symbolic)))
    (if (find-alias candidate env) ; the candidate is in scope
        candidate
        symbolic)))

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
  (for-each
   (lambda (name symbolic)
     ;; Canonizing here catches places where a parallel let binds the
     ;; same expression to multiple names.
     (let ((symbolic (cse-canonical env symbolic)))
       (if (acceptable-alias? symbolic)
           ;; Somewhere around here I can also accumulate information
           ;; of the form "this value has had the following names over
           ;; its lifetime".  Such information would be useful for
           ;; picking representative names to use for the final
           ;; output, so as to try and preserve the names originally
           ;; chosen by the user, for legibility of the output.  This
           ;; idea is due to Taylor Campbell.
           (hash-table/put! env name symbolic)
           (begin
             (hash-table/put! env name name)
             ;; Don't put variables back because if a variable is not
             ;; an acceptable alias, then it's out of scope, and
             ;; putting it back would forget that.  Also don't put
             ;; unique expressions in at all, because they represent
             ;; situations where repeating the same expression
             ;; (e.g. (read-real)) would have different effects.
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

(define (find-alias name env)
  (hash-table/lookup env name
    (lambda (datum)
      (cons name datum))
    (lambda () #f)))

;;; To do interprocedural must-alias analysis and alias elimination, I
;;; have to proceed as follows:
;;; 1) Initialize a cse-env for each user procedure, indicating which
;;;    formal parameters are aliases of each other.
;;;    - Start them all as treating all parameters as aliases (because
;;;      I have no evidence to the contrary yet).
;;; 2) Initialize a map for each procedure, producing an expression for
;;;    the return value(s) if given expressions for the parameters.
;;;    - I know the answer for primitives
;;;    - Start every user procedure mapping all outputs to being
;;;      aliases of the first input (or, for nullary functions, the
;;;      same fresh nullary (pure?) function term).
;;; 3) I can improve the map for any procedure (and the cse-envs of
;;;    all procedures it calls) by doing a CSE down the body of that
;;;    procedure, starting with that cse-env.
;;;    - At procedure calls, use the current map for the callee, and
;;;      intersect the cse-env of the callee with the normalized
;;;      expressions for the arguments passed in (thus discovering
;;;      situations where procedures are actually called with
;;;      different things).
;;;    - On return from the walk, update the map for the procedure
;;;      just walked; if needed, use generated function symbols
;;;      applied to the inputs (a function symbol would be an object
;;;      with a basename, in this case the name of the procedure just
;;;      walked, an index to indicate with of multiple returns it's
;;;      talking about (#f for unary returns), and a flag marking
;;;      whether the answer is pure.)
;;; 4) Repeat step 3 until the maps and environments stabilize.
;;; 5) Replace all procedure definitions to
;;;    - Accept only those parameters that are actually not aliases of
;;;      each other (internally LET-bind the dropped formals to the
;;;      canonical representatives)
;;;    - Return only those answers that are not aliases of each other
;;;      or the incoming formals (internally LET-VALUES everything the
;;;      body will generate, and VALUES out that which is wanted).
;;; 6) Replace all call sites to
;;;    - Supply only those arguments that are actually needed (just
;;;      drop the rest).
;;;    - Solicit only those outputs that are actually provided
;;;      (LET-VALUES them, and VALUES what the body expects, with
;;;      appropriate copying; for this to work, I think all the passed
;;;      parameters will need to be variables, so they can be reused).
;;; 7) Do a pass of intraprocedural CSE to clean up.  The only
;;;    information I need from the interprocedural step that is not
;;;    encoded in the rewrites is which slots in the output of some
;;;    procedure are pure and which are not.

;;; What would have to be done to extend the above to interprocedural
;;; CSE?  One improvement would be for the formal environments to
;;; track not just which things are aliases, but also whether some
;;; formal parameter actually stores an expression computed from the
;;; others.  That way, any internal recomuptation of that expression
;;; can be avoided; or, alternately, the computed input could not be
;;; passed across the procedure call, but recomputed by the callee (in
;;; which case it becomes accessible to intraprocedural CSE on the
;;; callee's end).  In the case of a recursive procedure implementing
;;; a tail-recursion loop, the effect of such a shift would be to move
;;; the computation of the expression from the point of self-call to
;;; the entry into procedure -- moving it out of CSE view in one place
;;; just as it moves into view in another (and also absorbing it from
;;; the actual initial caller into the loop itself).  That's why
;;; actual communication across the call boundary is important.

;;; An analagous improvements would be for the output expressions to
;;; expose the expressions that are hidden behind their function
;;; symbols, should any output be an expression in terms of the inputs
;;; or other outputs, so that the client can avoid recomputing it.
;;; Here, however, there lies a risk infinite regress, so the outputs
;;; must mask some expressions behind opaque symbols.  Which ones?  A
;;; limited version of this would only expose outputs that are
;;; expressions in terms of other outputs; the same considerations as
;;; for formals that are expressions in therms of other formals apply.
