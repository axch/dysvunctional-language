(declare (usual-integrations))
;;;; Optimization toplevel

;;; The FOL optimizer consists of several stages:
;;; - ALPHA-RENAME
;;;   Uniquify local variable names.
;;; - INLINE
;;;   Inline non-recursive function definitions.
;;; - SCALAR-REPLACE-AGGREGATES
;;;   Replace aggregates with scalars.
;;; - INTRAPROCEDURAL-CSE
;;;   Eliminate common subexpressions (including redundant variables
;;;   that are just aliases of other variables or constants).
;;;   Perform some algebraic simplification during CSE.
;;; - ELIMINATE-INTRAPROCEDURAL-DEAD-VARIABLES
;;;   Eliminate dead code.
;;; - INTERPROCEDURAL-DEAD-VARIABLE-ELIMINATION
;;;   Eliminate dead code across procedure boundaries.
;;; - REVERSE-ANF
;;;   Inline bindings of variables that are only used once, to make
;;;   the output easier to read.

;;; See doc/architecture.txt for discussion.

#|
;; Stage definitions might look like this

;; The properties are
;; syntax-checked
;; type
;; unique-names
;; a-normal-form
;; lets-lifted

;; The possible clause types are
;; requires
;; preserves generates destroys
;; computes
;; idempotent

;; TODO get property-generator to work
;; TODO get the autowrappers to wrap the property generators

(define-stage check-fol-types
  check-program-types
  (computes type)
  (generates syntax-checked))

(define-stage alpha-rename
  %alpha-rename
  (requires syntax-checked)
  (generates unique-names)
  (preserves a-normal-form lets-lifted type syntax-checked)
  (idempotent))

(define-stage inline
  %inline
  (requires syntax-checked)
  (preserves syntax-checked type a-normal-form) ; lets-lifted?
  (destroys unique-names)
  (idempotent))                  ; not really, but on current examples

(define-stage a-normal-form
  approximate-anf
  (requires syntax-checked)
  (generates a-normal-form)
  (preserves syntax-checked type unique-names)
  (destroys lets-lifted)
  (idempotent))

(define-stage lift-lets
  %lift-lets
  ;; The latter just because I'm lazy
  (requires syntax-checked a-normal-form)
  (generates lets-lifted)
  ;; TODO Does it really preserve a-normal-form ?
  (preserves syntax-checked type unique-names a-normal-form))

(define-stage scalar-replace-aggregates
  %scalar-replace-aggregates
  ;; TODO Does it require unique-names?
  (requires syntax-checked a-normal-form)
  (preserves syntax-checked type unique-names)
  (destroys a-normal-form lets-lifted)) ; because of the reconstruction

(define-stage intraprocedural-cse
  %intraprocedural-cse
  ;; The latter two requirements are not really requirements, but it
  ;; works much better this way.
  (requires syntax-checked unique-names a-normal-form lets-lifted)
  (preserves syntax-checked type unique-names a-normal-form lets-lifted)
  (idempotent))

(define-stage eliminate-intraprocedural-dead-code
  eliminate-intraprocedural-dead-variables
  ;; TODO Does it really require unique names?
  (requires syntax-checked unique-names)
  (preserves syntax-checked type unique-names a-normal-form lets-lifted)
  (idempotent))

(define-stage eliminate-interprocedural-dead-code
  ;; TODO Do I want to split this into the pure-interprocedural part
  ;; and the intraprocedural part?  One problem: the former
  ;; technically does not preserve type correctness, because it
  ;; inserts tombstones which currently do not type properly.  In
  ;; fact, this criticism can be levelled against the composition
  ;; well, in general.
  interprocedural-dead-code-elimination
  ;; TODO Does it really require unique names?
  (requires syntax-checked unique-names)
  (preserves syntax-checked type unique-names a-normal-form lets-lifted)
  (idempotent))
|#

(define (fol-optimize program)
  ((lambda (x) x) ; This makes the last stage show up in the stack sampler
   (reverse-anf
    (interprocedural-dead-code-elimination
     (eliminate-intraprocedural-dead-variables
      (intraprocedural-cse
       (scalar-replace-aggregates
        (inline                         ; includes ALPHA-RENAME
         program))))))))

;;; Watching the behavior of the optimizer

(define (optimize-visibly program)
  (report-size
   ((visible-stage reverse-anf)
    (interprocedural-dead-code-elimination-visibly
     ((visible-stage eliminate-intraprocedural-dead-variables)
      (intraprocedural-cse-visibly
       (scalar-replace-aggregates-visibly
        (inline-visibly                         ; includes ALPHA-RENAME
         program))))))))
