;;; ----------------------------------------------------------------------
;;; Copyright 2010-2011 National University of Ireland; 2012 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of DysVunctional Language.
;;; 
;;; DysVunctional Language is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Affero General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;;  License, or (at your option) any later version.
;;; 
;;; DysVunctional Language is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Affero General Public License
;;; along with DysVunctional Language.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))
;(declare (integrate-external "syntax")) ; Because SRA overrides CONSTRUCTION, below
(declare (integrate-external "../support/pattern-case/pattern-case"))
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

;;; For ease of bookkeeping, this implementation of SRA requires its
;;; input to be in approximate A-normal form, as produced by
;;; APPROXIMATE-ANF (see a-normal-form.scm, and the stage definition
;;; in optimize.scm).

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
;;;        ;; Also accessor procedures implied by DEFINE-TYPE
;;;        | (<proc-var> <simple-expression>)
;;;
;;; construction = (cons <simple-expression> <simple-expression>)
;;;              | (vector <simple-expression> ...)
;;;              ;; Also constructor procedures implied by DEFINE-TYPE
;;;              | (<proc-var> <simple-expression> ...)
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
  (let ((lookup-type (procedure-type-map program)))
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
                    (map invent-names-for-factors formals arg-shapes))
                   (env (augment-env
                         (empty-env) formals new-name-sets arg-shapes))
                   (new-names (apply append new-name-sets)))
              `(define (,name ,@new-names)
                 (argument-types
                  ,@(append-map atomic-fringe arg-shapes)
                  ,(tidy-values `(values ,@(atomic-fringe return))))
                 ,(sra-expression body env lookup-type
                                  (lambda (new-body shape) new-body))))))
    (fluid-let ((*accessor-constructor-map* (accessor-constructor-map program))
                (*type-map* (type-map program)))
      (if (begin-form? program)
          (append
           (map sra-definition (except-last-pair program))
           (list (sra-entry-point (last program))))
          (sra-entry-point program)))))

;; TODO this is another instance of the hack described in cse.scm.
(define *type-map* #f)

(define (sra-expression expr env lookup-type win)
  ;; An SRA environment maps every bound name to two things: the shape
  ;; it had before SRA and the list of names that have been assigned
  ;; by SRA to hold its primitive parts.  The list is parallel to the
  ;; fringe of the shape.  Note that the compound structure (vector)
  ;; has an empty list of primitive parts.
  ;; I need two pieces of information from the recursive call: the
  ;; new, SRA'd expression, and the shape of the value it used to
  ;; return before SRA.
  ;; For this purpose, a VALUES is the same as any other construction.
  (define (construction? expr)
    (or (and (pair? expr)
             (memq (car expr) '(cons vector values)))
        (implicit-construction? expr)))
  (define-algebraic-matcher construction construction? car cdr)
  (define (loop expr env)
    (case* expr
      ((fol-var _)
       (values (tidy-values `(values ,@(get-names expr env)))
               (get-shape expr env)))
      ((number _) (values expr 'real))
      ((boolean _) (values expr 'bool))
      ((null) (values `(values) '()))
      ((if-form pred cons alt)
       (sra-if pred cons alt env))
      ((let-form bindings body)
       (sra-let bindings body env))
      ((let-values-form names subexp body)
       (sra-let-values names subexp body env))
      ((lambda-form formals body)
       (sra-lambda formals body env))
      ((accessor _ _ _ :as expr) (sra-access expr env))
      ((construction ctor operands) (sra-construction ctor operands env))
      ((pair operator operands) ;; general application
       (sra-application operator operands env))))
  (define (sra-if pred cons alt env)
    (let-values (((new-pred pred-shape) (loop pred env))
                 ((new-cons cons-shape) (loop cons env))
                 ((new-alt   alt-shape) (loop alt  env)))
      (assert (eq? 'bool pred-shape))
      ;; TODO cons-shape and alt-shape better be the same
      ;; (or at least compatible)
      (values `(if ,new-pred ,new-cons ,new-alt)
              cons-shape)))
  (define (sra-let bindings body env)
    (receive (new-bind-expressions bind-shapes) (loop* (map cadr bindings) env)
      (let ((new-name-sets
             (map invent-names-for-factors
                  (map car bindings) bind-shapes)))
        (receive (new-body body-shape)
          (loop body (augment-env env (map car bindings) new-name-sets bind-shapes))
          (values (if (every (lambda (set)
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
                  body-shape)))))
  (define (sra-let-values names exp body env)
    (receive (new-exp exp-shape) (loop exp env)
      (let ((new-name-sets
             (map invent-names-for-factors names (sra-factors exp-shape))))
        ;; The previous line is not idempotent because it renames
        ;; all the bindings, even those that used to hold
        ;; non-structured values.
        (receive (new-body body-shape)
          (loop body (augment-env env names new-name-sets (sra-factors exp-shape)))
          (values (tidy-let-values
                   `(let-values ((,(apply append new-name-sets) ,new-exp))
                      ,new-body))
                  body-shape)))))
  (define (sra-lambda formals body env)
    ;; TODO Generalize to arg types other than real
    (receive (new-body body-shape)
      (loop body (augment-env env formals (list formals) (list 'real)))
      (values `(lambda ,formals
                 ,(reconstruct-pre-sra-shape new-body body-shape))
              #;(function-type 'real body-shape)
              'escaping-function)))
  (define (sra-access expr env)
    (receive (new-cadr cadr-shape) (loop (cadr expr) env)
      (let ((index (access-index expr))
            (factors (sra-factors cadr-shape)))
        (values (slice-values-by-index
                 index (smart-values-subforms new-cadr) factors)
                (list-ref factors index)))))
  (define (sra-construction ctor operands env)
    (receive (new-terms terms-shapes) (loop* operands env)
      (values (append-values new-terms)
              (construct-type terms-shapes ctor))))
  (define (sra-application operator operands env)
    (receive (new-args args-shapes) (loop* operands env)
      ;; The type checker should have ensured this
      ;(assert (every equal? args-shapes (arg-types (lookup-type operator)))
      (values `(,operator ,@(smart-values-subforms (append-values new-args)))
              (return-type (lookup-type operator)))))
  (define (loop* exprs env)
    (if (null? exprs)
        (values '() '())
        (let-values (((new-expr expr-shape) (loop (car exprs) env))
                     ((new-exprs expr-shapes) (loop* (cdr exprs) env)))
          (values (cons new-expr new-exprs)
                  (cons expr-shape expr-shapes)))))
  (receive (new-expr shape) (loop expr env) (win new-expr shape)))

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
  (define (walk shape names)
    (cond ((atomic-type? shape)
           (values (car names) (cdr names)))
          ((null? shape)
           (values '() names))
          ((construction? shape)
           (receive (new-exprs names-left) (walk* (cdr shape) names)
             (values `(,(car shape) ,@new-exprs) names-left)))
          (else
           (error "Weird shape" shape))))
  (define (walk* shapes-left names-left)
    (if (null? shapes-left)
        (values '() names-left)
        (receive (new-expr names-left) (walk (car shapes-left) names-left)
          (receive (new-exprs names-left) (walk* (cdr shapes-left) names-left)
            (values (cons new-expr new-exprs) names-left)))))
  (if (atomic-type? shape)
      new-expr
      (let ((piece-names (invent-names-for-factors 'receipt shape)))
        (tidy-let-values
         `(let-values ((,piece-names ,new-expr))
            ,(approximate-anf
              (receive (shape new-names) (walk shape piece-names)
                (assert (null? new-names))
                shape)))))))

;;; SRA supporting procedures

;; Type environments

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

;; Atomic expansions of given types

(define (count-atomic-factors shape)
  (cond ((null? shape) 0)
        ((atomic-type? shape) 1)
        (else (reduce + 0 (map count-atomic-factors (sra-factors shape))))))
(define (atomic-fringe shape)
  (cond ((null? shape) '())
        ((atomic-type? shape) (list shape))
        (else (append-map atomic-fringe (sra-factors shape)))))
(define (atomic-type? shape)
  ;; In due course, SRA may choose to leave some non-primitive types
  ;; unexpanded.
  (or (primitive-type? shape)
      (and *type-map*
           (escaping-function-type?
            (hash-table/get *type-map* shape #f)))))
(define (sra-factors shape)
  (if (atomic-type? shape)
      (list shape)
      (type-factors shape)))
(define (invent-names-for-factors basename shape)
  (let ((count (count-atomic-factors shape)))
    (if (= 1 count)
        (list basename)
        (map (lambda (i) (make-name basename))
             (iota count)))))
(define (slice-values-by-index index names shape)
  (tidy-values
   (let loop ((index-left index)
              (names-left names)
              (shape-left shape))
     (if (= 0 index-left)
         `(values ,@(take names-left
                          (count-atomic-factors (car shape-left))))
         (loop (- index-left 1)
               (drop names-left
                     (count-atomic-factors (car shape-left)))
               (cdr shape-left))))))
