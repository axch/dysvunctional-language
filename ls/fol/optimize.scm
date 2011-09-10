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

;;; Using the stage machinery, the fol optimizer looks like this

;; The properties are
;; syntax-checked
;; type
;; unique-names
;; a-normal-form
;; lets-lifted
;; fully-inlined  (for testing only)
;; aggregates-replaced (for testing only)
;; no-common-subexpressions (for testing only)

;; The possible clause types are
;; requires
;; preserves generates destroys
;; computes
;; idempotent

(define-stage check-fol-types
  check-program-types
  (computes type)
  (generates syntax-checked))

(define-stage alpha-rename
  %alpha-rename
  (generates unique-names)
  (preserves a-normal-form lets-lifted type syntax-checked fully-inlined aggregates-replaced no-common-subexpressions)
  (requires syntax-checked)
  (idempotent))

(define-stage inline
  %inline
  (preserves syntax-checked type a-normal-form aggregates-replaced)  ; lets-lifted?
  ;; Because of copying procedure bodies
  (destroys unique-names no-common-subexpressions)
  (requires syntax-checked)
  (generates fully-inlined)
  (idempotent))                  ; not really, but on current examples

(define-stage a-normal-form
  approximate-anf
  (generates a-normal-form)
  (preserves syntax-checked type unique-names fully-inlined aggregates-replaced)
  (destroys lets-lifted) ; Because of multiple argument procedures
  ;; By naming new things that may be common
  (destroys no-common-subexpressions)
  (requires syntax-checked)
  (idempotent))

(define-stage lift-lets
  %lift-lets
  (generates lets-lifted)
  ;; TODO Does it really preserve a-normal-form ?
  (preserves syntax-checked type unique-names a-normal-form fully-inlined aggregates-replaced)
  ;; The last just because I'm lazy
  (requires syntax-checked unique-names a-normal-form)
  ;; By splitting lets
  (destroys no-common-subexpressions))

(define (sra-may-destroy property)
  (modify-execution-function
   (lambda (exec)
     (lambda (program)
       (let ((val (property-value property program))
             (answer (exec program)))
         (if (memq (property-value 'type program) '(bool real))
             (property-value! property val answer)
             (property-value! property #f answer))
         answer)))))

(define-stage scalar-replace-aggregates
  %scalar-replace-aggregates
  (preserves syntax-checked type unique-names fully-inlined)
  ;; TODO Does it require unique-names?
  (requires syntax-checked a-normal-form)
  (generates aggregates-replaced)
  ;; Because of the reconstruction
  (sra-may-destroy a-normal-form aggregates-replaced)
  ;; Because of reconstruction and let-values simplification (?)
  (destroys lets-lifted)
  ;; By making aliases, and exposing structure slots to CSE
  (destroys no-common-subexpressions))

(define-stage intraprocedural-cse
  %intraprocedural-cse
  (preserves syntax-checked type unique-names a-normal-form lets-lifted fully-inlined aggregates-replaced)
  ;; The latter two requirements are not really requirements, but it
  ;; works much better this way.
  (requires syntax-checked unique-names a-normal-form lets-lifted)
  (generates no-common-subexpressions)
  (idempotent))

(define-stage eliminate-intraprocedural-dead-code
  eliminate-intraprocedural-dead-variables
  ;; Does not preserve fully-inlined because it may alter the call
  ;; graph
  ;; When there are union types, it may destroy aggregates-replaced
  ;; for the same reason.
  (preserves syntax-checked type unique-names a-normal-form lets-lifted aggregates-replaced no-common-subexpressions)
  ;; TODO Does it really require unique names?
  (requires syntax-checked unique-names)
  (idempotent))

(define-stage eliminate-interprocedural-dead-code
  ;; TODO Do I want to split this into the pure-interprocedural part
  ;; and the intraprocedural part?  One problem: the former
  ;; technically does not preserve type correctness, because it
  ;; inserts tombstones which currently do not type properly.  In
  ;; fact, this criticism can be levelled against the composition
  ;; well, in general.
  interprocedural-dead-code-elimination
  ;; Does not preserve fully-inlined because it may alter the call graph
  (preserves syntax-checked type unique-names a-normal-form lets-lifted aggregates-replaced no-common-subexpressions)
  ;; TODO Does it really require unique names?
  (requires syntax-checked unique-names)
  (idempotent))

(define-stage reverse-anf
  %reverse-anf
  (preserves syntax-checked type unique-names fully-inlined aggregates-replaced no-common-subexpressions) ; lets-lifted?
  (destroys a-normal-form)
  (requires syntax-checked unique-names)
  (idempotent))

(define fol-optimize
  (stage-pipeline
   reverse-anf
   eliminate-interprocedural-dead-code
   eliminate-intraprocedural-dead-code
   intraprocedural-cse
   scalar-replace-aggregates
   eliminate-intraprocedural-dead-code
   intraprocedural-cse
   inline))

(define (watching-annotations stage-data)
  (lambda (exec)
    (lambda (program)
      (display "(")
      (pp (stage-data-name stage-data))
      (let ((answer (exec program)))
        (pp (hash-table/get eq-properties answer #f))
        (display ")")
        answer))))

(define (visibly stage-data)
  (lambda (exec)
    (visible-named-stage exec (stage-data-name stage-data))))

(define optimize-visibly
  (do-stages fol-optimize visibly))
