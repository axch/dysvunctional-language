(declare (usual-integrations))
;;;; Optimization toplevel

;;; The FOL optimizer consists of several stages:
;;; - ALPHA-RENAME
;;;   Uniquify local variable names.
;;; - INLINE
;;;   Inline non-recursive function definitions.
;;; - SCALAR-REPLACE-AGGREGATES
;;;   Replace aggregates with scalars.
;;; - INTRAPROCEDURAL-DE-ALIAS
;;;   Eliminate redundant variables that are just aliases of other
;;;   variables or constants.
;;; - INTRAPROCEDURAL-DEAD-VARIABLE-ELIMINATION
;;;   Eliminate dead code.
;;; - TIDY
;;;   Clean up and optimize locally by term-rewriting.  This includes
;;;   a simple-minded reverse-anf which inlines bindings of variables
;;;   that are only used once, to make the output easier to read.

(define (fol-optimize program)
  ((lambda (x) x) ; This makes the last stage show up in the stack sampler
   (tidy
    (interprocedural-dead-code-elimination
     (eliminate-intraprocedural-dead-variables
      (intraprocedural-de-alias
       (scalar-replace-aggregates
        (inline                         ; includes ALPHA-RENAME
         program))))))))

;;; The stages have the following structure and interrelationships:
;;;
;;; Almost all other stages (notably except INLINE) depend
;;; on but also preserve uniqueness of variable names, so ALPHA-RENAME
;;; should be done first.  The definition of FOL-OPTIMIZE above bums
;;; this by noting that INLINE is called first anyway and relying on
;;; the ALPHA-RENAME inside it.
;;;
;;; Other than that, any stage is valid at any point, so the order and
;;; frequency of calling them is a question of their idempotence, what
;;; opportunities they expose for each other, and whether they give
;;; each other any excess work.  The following table summarizes these
;;; relationships.
#|
|          | Inline        | SRA         | de-alias  | dead var | tidy   |
|----------+---------------+-------------+-----------+----------+--------|
| Inline   | almost idem   | no effect   | expose    | expose   | expose |
| SRA      | extra aliases | almost idem | expose    | expose   | expose |
| de-alias | no effect     | no effect   | idem      | expose   | expose |
| dead var | ~ expose      | no effect   | no effect | idem     | expose |
| tidy     | ~ expose      | form fight  | ~ expose  | ~ expose | idem   |
|#
;;; Each cell in the table says what effect doing the stage on the
;;; left first has on subsequently doing the stage above.  "Expose"
;;; means that the stage on the left exposes opportunities for the
;;; stage above to be more effective.  "Idem" means the stage is
;;; idempotent, that is that repeating it twice in a row is no better
;;; than doing it once.  "~ expose" means it exposes opportunities in
;;; principle, but the current set of examples has not yet motivated
;;; me to try to take advantage of this.  I explain each cell
;;; individually below.

;;; Scalar replacement of aggregates pre-converts its input into
;;; approximate A-normal form, and does not attempt to undo this
;;; conversion.  This means other stages may have interesting
;;; commutators with SCALAR-REPLACE-AGGREGATES through their effect on
;;; ANF.
;;;
;;; Inline then Inline: Inlining is not idempotent in theory (see
;;; discussion in feedback-vertex-set.scm) but is idempotent on the
;;; extant examples.
;;;
;;; Inline then SRA: Inlining commutes with SRA up to removal of
;;; aliases (see explanation in SRA then Inline below).  I think
;;; inlining also makes SRA go faster because it reduces the number of
;;; procedure boundaries.
;;;
;;; Inline then others: Inlining exposes some interprocedural aliases,
;;; dead code, and tidying opportunities to intraprocedural methods by
;;; collapsing some procedure boundaries.
;;;
;;; SRA then Inline: Inlining gives explicit names (former formal
;;; parameters) to the argument expressions of the procedure calls
;;; that are inlined, whether those expressions are compound or not.
;;; The ANF pre-filter of SRA synthesizes explicit names for any
;;; compound expression, including arguments of procedures that are up
;;; for inlining.  Therefore, doing SRA first creates extra names that
;;; just become aliases after inlining.  Up to removal of aliases,
;;; however, SRA and inlining commute.
;;;
;;; SRA then SRA: SRA is idempotent except in the case when the entry
;;; point returns a structured object (see sra.scm for why).  When
;;; support for union types is added, SRA will also become
;;; non-idempotent for the same reason that inlining is not
;;; idempotent.
;;; 
;;; SRA then others: SRA converts structure slots to variables,
;;; thereby exposing any aliases, dead code, or tidying opportunities
;;; over those structure slots to the other stages, which focus
;;; exclusively on variables.
;;;
;;; De-alias then others: De-aliasing is idempotent, does not create
;;; inlining opportunities, and does not create SRA opportunities.
;;;
;;; De-alias then eliminate: Formally, the job of de-aliasing is
;;; just to rename references to aliases to refer to the objects they
;;; are aliases of, so that the bindings of the aliases themselves can
;;; be cleaned up by dead variable elimination.  The particular
;;; de-aliasing program implemented here opportunistically eliminates
;;; most of those alias bindings itself, but it does leave a few
;;; aliases around to be cleaned up by dead variable elimination, in
;;; the case where some names bound by a multiple value binding form
;;; are aliases but others are not.
;;;
;;; De-alias then tidy: De-aliasing exposes tidying opportunities, for
;;; example by constant propagation.
;;;
;;; Eliminate then inline: Dead variable elimination may delete edges
;;; in the call graph (if the result of a called procedure turned out
;;; not to be used); and may thus open inlining opportunities.
;;;
;;; Eliminate then SRA: Dead variable elimination does not create SRA
;;; opportunities (though it could in the non-union-free case if I
;;; eliminated dead structures or structure slots and were willing to
;;; change the type graph accordingly).
;;;
;;; Eliminate then de-alias: Dead variable elimination does not expose
;;; aliases.
;;;
;;; Eliminate then eliminate: Dead variable elimination is idempotent.
;;;
;;; Eliminate then tidy: Dead variable elimination exposes tidying
;;; opportunities, for example by collapsing intervening LETs or by
;;; making bindings singletons.
;;;
;;; Tidy then inline: Tidying may delete edges in the call graph by
;;; collapsing (* 0 (some-proc foo bar baz)) to 0 or by collapsing
;;; (if (some-proc foo) bar bar) into bar.
;;;
;;; Tidy then SRA: Tidying does not create SRA opportunities (though
;;; it could in the non-union-free case).  It does, however, do some
;;; reverse ANF-conversion, in the form of inlining variables that are
;;; only used once.  Consequently, SRA and tidying could fight
;;; indefinitely over the "normal form" of a program, each appearing
;;; to change it while neither doing anything useful.
;;;
;;; Tidy then de-alias: Tidying may expose aliases by removing
;;; intervening arithmetic.  For example, in (let ((x (+ 0 y))) ...),
;;; x would not register as an alias of y until after tidying (+ 0 y)
;;; to y.
;;;
;;; Tidy then eliminate: Tidying may expose dead variables by
;;; collapsing (* 0 foo) to 0 or by collapsing (if foo bar bar) to
;;; bar.
;;;
;;; Tidy then tidy: Tidying is idempotent (because it is run to
;;; convergence).

;;; Don't worry about the rule-based term-rewriting system that powers
;;; this.  That is its own pile of stuff, good for a few lectures of
;;; Sussman's MIT class Adventures in Advanced Symbolic Programming.
;;; It works, and it's very good for peephole manipulations of
;;; structured expressions (like the output of the VL code generator).
;;; If you really want to see it, though, it's included in
;;; support/rule-system.

;;; Rules for the term-rewriting system consist of a pattern to try to
;;; match and an expression to evaluate to compute a replacement for
;;; that match should a match be found.  Patterns match themselves;
;;; the construct (? name) introduces a pattern variable named name;
;;; the construct (? name ,predicate) is a restricted pattern variable
;;; which only matches things the predicate accepts; the construct (??
;;; name) introduces a sublist pattern variable.  The pattern matcher
;;; will search through possible lengths of sublists to find a match.
;;; Repeated pattern variables must match equal structures in all the
;;; corresponding places.

;;; A rule by itself is a one-argument procedure that tries to match
;;; its pattern.  If the match succeeds, the rule will evaluate the
;;; the replacement expression in an environment where the pattern
;;; variables are bound to the things they matched and return the
;;; result.  If the replacement expression returns #f, that tells the
;;; matcher to backtrack and look for another match.  If the match
;;; fails, the rule will return #f.

;;; A rule simplifier has a set of rules, and applies them to every
;;; subexpression of the input expression repeatedly until the result
;;; settles down.

;;;; Term-rewriting tidier

(define (tidy-begin form)
  (if (and (begin-form? form) (null? (cddr form)))
      (cadr form)
      form))
(define empty-let-rule (rule `(let () (? body)) body))

;; This is safe assuming the program has unique bound names; if not,
;; can break because of
#;
 (let ((x 3))
   (let ((y (let ((x 4)) x)))
     x))
;; Unfortunately, mere lack of shadowing is not enough, as this can
;; introduce shadowing because of
#;
 (let ((x (let ((y 3)) y)))
   (let ((y 4))
     y))
(define let-let-lifting-rule
  (rule `(let ((?? bindings1)
               ((? name ,fol-var?) (let (? in-bindings) (? exp)))
               (?? bindings2))
           (?? body))
        `(let ,in-bindings
           (let (,@bindings1
                 (,name ,exp)
                 ,@bindings2)
             ,@body))))

;; This is safe assuming the program has been alpha renamed
(define values-let-lifting-rule
  (rule `(let-values (((? names) (let (? in-bindings) (? exp))))
           (?? body))
        `(let ,in-bindings
           (let-values ((,names ,exp))
             ,@body))))

;; This is safe assuming the program has been alpha renamed
(define singleton-inlining-rule
  (rule `(let ((?? bindings1)
               ((? name ,fol-var?) (? exp))
               (?? bindings2))
           (?? body))
        (let ((occurrence-count (count-free-occurrences name body)))
          (and (= 1 occurrence-count)
               `(let (,@bindings1
                      ,@bindings2)
                  ,@(replace-free-occurrences name exp body))))))

(define tidy
  (rule-simplifier
   (list
    tidy-begin

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

    (rule `(if (? predicate) (? exp) (? exp))
          exp)

    (rule `(let-values (((? names) (values (?? stuff))))
             (?? body))
          `(let ,(map list names stuff)
             ,@body))

    empty-let-rule

    values-let-lifting-rule
    singleton-inlining-rule)))

;;; Watching the behavior of the optimizer

(define-syntax fol-stage
  (syntax-rules ()
    ((_ name)
     (fol-named-stage name 'name))))

(define (fol-named-stage stage name)
  (lambda (input)
    (display "Stage ")
    (display name)
    (display " on ")
    (display (count-pairs input))
    (display " pairs")
    (newline)
    (let ((answer (show-time (lambda () (stage input)))))
      (newline)
      answer)))

(define (report-size program)
  (display "Final output has ")
  (display (count-pairs program))
  (display " pairs")
  (newline)
  program)

(define (compile-visibly program)
  (report-size ; This makes the last stage show up in the stack sampler
   ((fol-stage tidy)
    ((fol-stage interprocedural-dead-code-elimination)
     ((fol-stage eliminate-intraprocedural-dead-variables)
      ((fol-stage intraprocedural-de-alias)
       ((fol-stage scalar-replace-aggregates)
        ((fol-stage inline)             ; includes ALPHA-RENAME
         ((fol-stage structure-definitions->vectors)
          (let ((raw-fol ((fol-stage analyze-and-generate)
                          program)))
            (clear-name-caches!)
            raw-fol))))))))))

;;;; Compilation with MIT Scheme

(define (fol->standalone-mit-scheme program #!optional output-base)
  (if (default-object? output-base)
      (set! output-base "frobnozzle"))
  (let* ((srfi-11 (read-source "../vl/fol/srfi-11.scm"))
         (runtime (read-source "../vl/fol/runtime.scm"))
         (output
          `(,@srfi-11                   ; includes usual-integrations
            ,@(cdr runtime)             ; remove usual-integrations
            ,@(cdr program)             ; remove begin
            )))
    (with-output-to-file (string-append output-base ".scm")
      (lambda ()
        (for-each (lambda (form)
                    (pp form)
                    (newline)) output)))
    (cf output-base)
    ;; TODO Actually loading this into the currently running Scheme
    ;; redefines the gensym structure, leading to trouble.
    (load output-base)))

(define (internalize-definitions program)
  (if (begin-form? program)
      `(let () ,@(cdr program))
      program))

(define (fol->mit-scheme program #!optional output-base)
  (if (default-object? output-base)
      (set! output-base "frobnozzle"))
  (let ((output `((declare (usual-integrations))
                  ,(internalize-definitions program))))
    (with-output-to-file (string-append output-base ".scm")
      (lambda ()
        (for-each (lambda (form)
                    (pp form)
                    (newline)) output)))
    (fluid-let ((sf/default-syntax-table (nearest-repl/environment)))
      (cf output-base))))

(define (run-mit-scheme #!optional output-base)
  (if (default-object? output-base)
      (set! output-base "frobnozzle"))
  (load output-base))

(define (flonumize program)
  (define floating-versions
    (cons (cons 'atan 'flo:atan2)
          (map (lambda (name)
                 (cons name (symbol 'flo: name)))
               '(+ - * / < = > <= >= abs exp log sin cos tan asin acos
                   sqrt expt zero? negative? positive?))))
  (define (arithmetic? expr)
    (and (pair? expr)
         (assq (car expr) floating-versions)))
  (define (real-call? expr)
    (and (pair? expr)
         (eq? (car expr) 'real)))
  (define read-real-call? (tagged-list? 'read-real))
  (define (replace thing)
    (cdr (assq thing floating-versions)))
  (define (loop expr)
    (cond ((number? expr) (exact->inexact expr))
          ((access? expr) `(,(car expr) ,(loop (cadr expr)) ,@(cddr expr)))
          ((arithmetic? expr)
           `(,(replace (car expr)) ,@(map loop (cdr expr))))
          ((real-call? expr)
           (loop (cadr expr)))
          ((read-real-call? expr)
           `(exact->inexact ,expr))
          ((pair? expr) (map loop expr))
          (else expr)))
  (loop program))

;;; Manually integrating arithmetic to avoid binding variables to hold
;;; intermediate floating point values, in the hopes that this will
;;; improve compiled floating point performance in MIT Scheme.  It did
;;; not, however, appear to have worked.
(define integrate-arithmetic
  (let ((arithmetic?
         (lambda (symbol)
           (memq symbol '(+ - * / < = > <= >= abs exp log sin cos tan asin acos
                            atan sqrt expt zero? negative? positive? real)))))
    (define (all-arithmetic? expr)
      (cond ((pair? expr)
             (and (arithmetic? (car expr))
                  (every all-arithmetic? (cdr expr))))
            ((number? expr) #t)
            ((fol-var? expr) #t)
            (else #f)))
    (rule-simplifier
     (list
      empty-let-rule
      (rule `(let ((?? bindings1)
                   ((? name ,fol-var?) (? exp ,all-arithmetic?))
                   (?? bindings2))
               (?? body))
            `(let (,@bindings1
                   ,@bindings2)
               ,@(replace-free-occurrences name exp body)))))))

(define (fol->floating-mit-scheme program #!optional output-base)
  (fol->mit-scheme (flonumize program) output-base))
