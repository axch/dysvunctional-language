(declare (usual-integrations))
(declare (integrate-external "syntax"))
(declare (integrate-external "../support/pattern-matching"))
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
;;; substantially hairier (discussion at the end of this file).  I
;;; will probably need it eventually (to eliminate interprocedural
;;; aliases, if nothing else), but I can do without for now.

;;; Intraprocedural CSE is a structural recursion over the code,
;;; walking LET bindings before LET bodies.  The recursion carries
;;; down an environment mapping every computed expression to the
;;; canonical variable storing the result of that expression.  The
;;; recursive call returns the CSE'd subexpression and also the
;;; (canonized) expression said subexpression computes, suitable for
;;; use as a key in the environment (there is ample potential for
;;; algebraic simplification here, but beware floating point).

;;; The grammar of symbolic expressions, assuming the input is in
;;; approximate A-normal form, is:
;;;
;;; <canonical> = const | var
;;; <symbolic> = <canonical>
;;;            | (if <symbolic> <symbolic> <symbolic>)
;;;            | (values <canonical> ...)
;;;            | (constructor <canonical> ...)
;;;            | (accessor <canonical> ...) ; Includes values-ref
;;;            | (proc-var <canonical> ...)
;;;            | unique-tag
;;;
;;; These are just like FOL expressions, except that LETs are elided,
;;; and there is an extra possibility, called the unique-tag.  This
;;; tag is a sentinel meaning "this expression is not pure, so do not
;;; eliminate copies of it".  Symbolic expressions also admit a
;;; synthetic accessor named VALUES-REF for representing the
;;; destructuring that LET-VALUES does on the return of its
;;; expression.

;;; If the input is not in approximate ANF, the grammar of symbolic
;;; expressions becomes
;;;
;;; <canonical> = const | var
;;; <symbolic> = <canonical>
;;;            | (if <symbolic> <symbolic> <symbolic>)
;;;            | (values <symbolic> ...)
;;;            | (constructor <symbolic> ...)
;;;            | (accessor <symbolic> ...) ; Includes values-ref
;;;            | (proc-var <symbolic> ...)
;;;            | unique-tag

;;; Invariants of the environment structure:
;;; - Every key is a symbolic expression and every datum is the
;;;   canonical name for that object (a variable or a constant).
;;;   Future occurrences of that expression will be replaced by a
;;;   reference to that canonical name, as long as the name remains in
;;;   scope.
;;; - Corollary: A variable key that has itself for a datum is a
;;;   canonical name (for something).
;;; - Corollary: A variable key that has some other variable or
;;;   constant for a datum is an alias.
;;; - A variable that does not occur in the table as a key is not in
;;;   scope.
;;; - An entry whose datum is not in scope is treated as though it
;;;   does not exist (this is implemented by the procedure
;;;   CSE-CANONICAL).
;;; - Every variable is the canonical name for at most one
;;;   non-variable expression.
;;; - An expression that does not occur as a key is fresh, in the
;;;   sense that no in-scope variable already stores the value this
;;;   expression would compute.
;;; - The unique-tag expression never occurs as a key (because it is
;;;   explicitly never inserted).

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
;;; - Constructions are analagous to procedures.  This has the effect
;;;   of statically recovering structure sharing in data structures
;;;   built within one procedure.
;;; - Accesses are also analagous to procedures, with the added bonus
;;;   that if they access an object that was constructed in the same
;;;   procedure, they can be simplified to the obvious thing.  This
;;;   requires reverse lookups in the environment.

;;; Because this is an intraprocedural common subexpression
;;; elimination, user procedures are not studied, but always assumed
;;; to be impure; and incoming procedure formal parameters are always
;;; assumed to be their own canonical variables (in particular, not
;;; aliases of each other).  This whole thing will work better if the
;;; input is in A-normal form, because intermediate values will always
;;; get names, and there will be a maximum of opportunities for
;;; detecting commonalities.  It will also work a whole lot better if
;;; all lets are lifted, because then previously computed
;;; subexpressions will spend more time in scope.

(define (%intraprocedural-cse program)
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
      (fluid-let ((*accessor-constructor-map* (accessor-constructor-map program))
                  (*implicit-pure-procedures* (map car (implicit-procedures (type-map program)))))
        (append
         (map cse-definition (except-last-pair program))
         (list (cse-entry-point (last program)))))
      (cse-entry-point program)))

;;; TODO These global variables are horrible hacks, added because in
;;; the presence of user structures, the set of accessors and
;;; constructors is open; but I don't want to modify the existing code
;;; to carry that information until I know I am using it properly.
(define *accessor-constructor-map* #f)
(define *implicit-pure-procedures* #f)

(define (cse-expression expr env)
  ;; A CSE environment maps every symbolic expression to the name of
  ;; the canonical variable that holds that expression, if any (notes:
  ;; an expression whose value is a known constant will be mapped to
  ;; that constant; a variable may be mapped to itself to indicate
  ;; that it holds something that no previously computed variable
  ;; holds).  It is important to know when such an environment does
  ;; not bind a variable at all; that means that variable in not in
  ;; scope here (constants are always in scope).
  ;;
  ;; Recursive calls return two things: the
  ;; new, CSE'd expression and the symbolic expression representing
  ;; the return value from this expression (using a values form for
  ;; the symbolic expressions for the elements of a multivalue
  ;; return).  If this expression is unique and gets bound to
  ;; something, that something will become the canonical name for this
  ;; expression.  The symbolic expression is actually very similar to
  ;; the returned CSE'd expression, except that the symbolic
  ;; expression elides LET forms.  Uniqueness of names and care about
  ;; scopes ensure this elision causes no trouble.
  (define (loop expr env)
    (case* expr
      ((fol-var _) (cse-fol-var expr env))
      ((fol-const _) (values expr expr))
      ((if-form pred cons alt) (cse-if pred cons alt env))
      ((let-form bindings body) (cse-let bindings body env))
      ((let-values-form names exp body)
       (cse-let-values names exp body env))
      ((lambda-form formals body) (cse-lambda formals body env))
      ((values-form subforms) (cse-values subforms env))
      ((pair operator operands) ; general application
       (cse-application operator operands env))))
  (define (cse-fol-var expr env)
    (forward-lookup env expr
     (lambda (canonical)
       (values canonical canonical))
     (lambda ()
       (error "Trying to cse an unbound variable" expr env))))
  (define (cse-if pred cons alt env)
    (let-values (((new-pred symbolic-pred) (loop pred env))
                 ((new-cons symbolic-cons) (loop cons env))
                 ((new-alt  symbolic-alt ) (loop alt env)))
      ;; TODO SYMBOLIC-IF detects cases where both branches of
      ;; the IF are equal.  Do I want to simplify the returned
      ;; IF expression in that case?
      (values `(if ,new-pred ,new-cons ,new-alt)
              (symbolic-if symbolic-pred symbolic-cons symbolic-alt))))
  (define (cse-let bindings body env)
    (let*-values (((new-bind-expressions bound-symbolics) (loop* (map cadr bindings) env))
                  ((env dead-bindings) (augment-cse-env env (map car bindings) bound-symbolics))
                  ((new-body body-symbolic) (loop body env)))
      (degment-cse-env! env (map car bindings))
      (values (tidy-empty-let
               `(let ,(filter-map
                       (lambda (name dead? expr)
                         (and (not dead?)
                              (list name expr)))
                       (map car bindings)
                       dead-bindings
                       new-bind-expressions)
                  ,new-body))
              body-symbolic)))
  (define (cse-let-values names subexpr body env)
    (let*-values (((new-subexpr subexpr-symbolic) (loop subexpr env))
                  ((env dead-bindings)
                   (augment-cse-env env names
                    (cond ((values-form? subexpr-symbolic)
                           (cdr subexpr-symbolic))
                          ;; If the symbolic expression is not a values form, it
                          ;; must be a procedure call that returns multiple
                          ;; values.  I can still represent the operation of
                          ;; destructuring it, on the off chance that the same
                          ;; procedure may be called again.  This is only safe
                          ;; if the procedure is known to be pure.
                          ((not (unique-expression? subexpr-symbolic))
                           (map (lambda (i)
                                  `(values-ref ,subexpr-symbolic ,i))
                                (iota (length names))))
                          (else
                           ;; If the subexpression is unique, so are all the
                           ;; variables being bound now.
                           (make-list (length names) unique-expression)))))
                  ((new-body body-symbolic) (loop body env)))
      ;; DEAD-BINDINGS tells me which of these bindings
      ;; are guaranteed to be dead because the variables
      ;; being bound are aliases and have already been
      ;; replaced in the new body.  I could eliminate them,
      ;; but that would require traversing subexpr again to
      ;; look for the VALUES that supplies the corresponding
      ;; values.  For now, I will just kill the whole
      ;; let-values if it is useless.
      (degment-cse-env! env names)
      (values (if (any not dead-bindings)
                  `(let-values ((,names ,new-subexpr))
                     ,new-body)
                  new-body)
              body-symbolic)))
  (define (cse-lambda formals body env)
    (let*-values (((env dead-bindings) (augment-cse-env env formals (list unique-expression)))
                  ((new-body body-symbolic) (loop body env)))
      (degment-cse-env! env formals)
      (values `(lambda ,formals
                 ,new-body)
              ;; TODO With more work, I could try to collapse
              ;; identical exported functions, but why bother?
              unique-expression)))
  (define (cse-values subexprs env)
    (receive (exprs symbolics) (loop* subexprs env)
      (values `(values ,@exprs) `(values ,@symbolics))))
  (define (cse-application operator operands env)
    (receive (new-args args-symbolics) (loop* operands env)
      (let* ((symb-candidate
              (symbolic-application operator args-symbolics env))
             (symbolic (cse-canonical env symb-candidate)))
        (values (if (or (and (fol-var? symbolic)
                             (in-scope? symbolic env))
                        (fol-const? symbolic))
                    symbolic
                    (simplify-arithmetic `(,operator ,@new-args)))
                symbolic))))
  (define (loop* exprs env)
    (if (null? exprs)
        (values '() '())
        (let-values (((new-expr expr-symbolic) (loop (car exprs) env))
                     ((new-exprs expr-symbolics) (loop* (cdr exprs) env)))
          (values (cons new-expr new-exprs)
                  (cons expr-symbolic expr-symbolics)))))
  (receive (new-expr symbolic) (loop expr env)
    ;; The symbolic expression might be useful to an
    ;; interprocedural CSE crunch.
    new-expr))

(define simplify-arithmetic
  ;; There are lots of possibilities for simplification here,
  ;; especially if I request the environment and look up the
  ;; symbolic expressions that various variables among the arguments
  ;; hold.
  ;;
  ;; For some warnings, see doc/simplification.txt
  ;;
  ;; TODO Do I want to notice that (+ 2 x) is the same as (+ x 2)?
  ;; TODO (* -1 (* -1 x)) -> x appears in celestial.dvl
  ;; TODO (+ x (* -1 y)) -> (- x y) appears in celestial.dvl
  ;; TODO (/ (* x stuff) (* x other stuff)) should appear in celestial.dvl
  (rule-simplifier
   (list
    (rule `(+ 0 (? thing)) thing)
    (rule `(+ (? thing) 0) thing)
    (rule `(- (? thing) 0) thing)

    (rule `(* 1 (? thing)) thing)
    (rule `(* (? thing) 1) thing)
    (rule `(/ (? thing) 1) thing)

    ;; Warning: these may replace a runtime NaN answer with 0.
    (rule `(* 0 (? thing)) 0)
    (rule `(* (? thing) 0) 0)
    (rule `(/ 0 (? thing)) 0)
    )))

(define (symbolic-application operator arguments env)
  (define (simplify-access expr)
    (if (accessor? expr)
        (let ((accessee (cadr expr)))
          ;; Let's see what value this accessed object holds.  I think
          ;; this relies on the accessee being a single variable, and
          ;; furthermore the canonical name of the constructed object
          ;; being accessed.
          (reverse-lookup env accessee
           (lambda (held-value)
             (if (construction? held-value)
                 ;; Assume it was a construction of the same type as
                 ;; is accessed, because the type checker should
                 ;; ensure this.
                 (let ((candidate
                        ;; It's not really "from shape" here, but
                        ;; let's pick out the accessed field.
                        (select-by-access held-value expr)))
                   ;; If the canonical name for that accessed field is
                   ;; still in scope, then the current expression can
                   ;; be replaced by it.
                   (let ((canonical-candidate
                          (forward-get env candidate candidate)))
                     (if (in-scope? canonical-candidate env)
                         canonical-candidate
                         expr)))
                 expr))
           (lambda ()
             ;; The held value was not constructed in this procedure
             expr)))
        expr))
  ;; Treating constructors and accessors as non-user-procedures (and
  ;; as not impure) here has the effect of collapsing EQUAL?
  ;; structures into EQ? structures.  This is ok because identity of
  ;; structures is not testable in the source language.
  (define (simplify expr)
    (simplify-arithmetic (simplify-access expr)))
  (define (user-procedure? operator)
    (not (or (memq operator (append '(cons car cdr vector vector-ref)
                                    (map primitive-name *primitives*)))
             (and *implicit-pure-procedures*
                  (memq operator *implicit-pure-procedures*)))))
  (simplify
   (cond ((memq operator
                (cons 'real ; REAL is "statically" but not "dynamically" impure
                      (map primitive-name
                           (filter primitive-impure? *primitives*))))
          unique-expression)
         ((user-procedure? operator)    ; TODO This is painful.
          unique-expression)
         ((any unique-expression? arguments)
          ;; This is defensive, but not optimal for constructions.  It
          ;; prevents optimization of (car (cons x (read-real))), for
          ;; instance.  Then again, converting to approximate ANF
          ;; first should prevent this case from ever happening.
          unique-expression)
         (else `(,operator ,@arguments)))))

(define (symbolic-if pred cons alt)
  (cond ((equal? cons alt)
         ;; This is somewhat dangerous.  See doc/simplification.txt.
         cons)
        ((and (values-form? cons)
              (values-form? alt))
         `(values ,@(map (lambda (sub-cons sub-alt)
                           (symbolic-if pred sub-cons sub-alt))
                         (cdr cons) (cdr alt))))
        ((or (unique-expression? pred)
             (unique-expression? cons)
             (unique-expression? alt))
         unique-expression)
        (else
         `(if ,pred ,cons ,alt))))

(define-structure (unique-expression safe-accessors))
(define unique-expression (make-unique-expression))

(define (cse-canonical env symbolic)
  ;; Either here or in degment-cse-env!, I have to flush expressions
  ;; whose canonical variables are not in scope any more.
  (let ((candidate (forward-get env symbolic symbolic)))
    (if (in-scope? candidate env)
        candidate
        symbolic)))

(define (empty-cse-env)
  (make-two-way-table
   (lambda (key datum)
     (not (fol-var? key)))))

(define (fresh-cse-env names)
  (receive (env dead-bindings) (augment-cse-env (empty-cse-env) names names)
     env))

(define (augment-cse-env env names symbolics)
  (define (acceptable-alias? symbolic)
    (or (fol-const? symbolic)
        (and (fol-var? symbolic)
             (in-scope? symbolic env))))
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
           (two-way-put! env name symbolic)
           (begin
             ;; The symbolic expression had no canonical name.
             ;; Therefore the given name becomes a canonical name for
             ;; itself, and maybe for the expression.
             (two-way-put! env name name)
             ;; Don't put variables back because if a variable is not
             ;; an acceptable alias, then it's out of scope, and
             ;; putting it back would forget that.  Also don't put
             ;; unique expressions in at all, because they represent
             ;; situations where repeating the same expression
             ;; (e.g. (read-real)) would have different effects.
             (if (and (not (fol-var? symbolic))
                      (not (unique-expression? symbolic)))
                 (two-way-put! env symbolic name))))))
   names
   symbolics)
  (values env (map acceptable-alias? symbolics)))

(define (degment-cse-env! env names)
  (for-each (lambda (name)
              (two-way-remove! env name))
            names))

(define (in-scope? name env)
  (or (fol-const? name)
      ;; This #f default is OK because #f is never the value of a
      ;; non-constant canonical name (which is because #f, being
      ;; constant, is always its own canonical name).
      (forward-get env name #f)))

;;; TODO compare structures-map in structs.scm and implicit-procedures
;;; in type-check.scm.
(define (accessor-constructor-map program)
  (if (begin-form? program)
      (let* ((structure-definitions (filter structure-definition? program))
             (structure-names (map cadr structure-definitions))
             (structure-map (make-eq-hash-table)))
        (hash-table/put-alist!
         structure-map
         (map (lambda-case*
               ((structure-definition name _)
                (cons (symbol 'make- name) 'constructor)))
              structure-definitions))
        (hash-table/put-alist!
         structure-map
         (append-map
          (lambda-case*
           ((structure-definition name fields)
            (map (lambda (field index)
                   (cons (symbol name '- field) index))
                 (map car fields)
                 (iota (length fields)))))
          structure-definitions))
        (define (classify name)
          (hash-table/get structure-map name #f))
        classify)
      (lambda (x) #f)))

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
