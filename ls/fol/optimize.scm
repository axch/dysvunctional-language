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
;; structures-as-vectors
;; syntax-checked
;; type
;; unique-names
;; a-normal-form
;; lets-lifted
;; fully-inlined  (for testing only)
;; aggregates-replaced (for testing only)
;; no-common-subexpressions (for testing only)
;; no-intraprocedural-dead-variables (for testing only)
;; no-interprocedural-dead-variables (for testing only)

;; The possible clause types are
;; preserves generates destroys
;; requires
;; computes reads
;; name

(define-stage structure-definitions->vectors
  %structure-definitions->vectors
  (preserves type) ; Because closures are never exported anyway
  ;; Because different closure types may collapse
  (destroys no-common-subexpressions)
  ;; Because SRA (currently) operates only on vectors and conses
  (destroys aggregates-replaced)
  (generates structures-as-vectors))

(define-stage check-fol-types
  check-program-types
  (computes type)
  (generates syntax-checked)
  (requires structures-as-vectors))

(define-stage alpha-rename
  %alpha-rename
  (generates unique-names)
  (requires syntax-checked))

(define-stage inline
  %inline
  ;; Because of multiple procedure arguments
  (destroys lets-lifted)
  ;; Because of copying procedure bodies
  (destroys unique-names no-common-subexpressions)
  ;; Because of removing procedure boundaries
  (destroys no-intraprocedural-dead-variables)
  ;; Because of specializing to different places
  (destroys no-interprocedural-dead-variables)
  (requires syntax-checked)
  (generates fully-inlined))                  ; not really, but on current examples

(define-stage a-normal-form
  approximate-anf
  (generates a-normal-form)
  (destroys lets-lifted) ; Because of multiple argument procedures
  ;; By naming new things that may be common
  (destroys no-common-subexpressions)
  (requires syntax-checked))

(define-stage lift-lets
  %lift-lets
  (generates lets-lifted)
  ;; TODO Does it really preserve a-normal-form ?
  (requires syntax-checked unique-names)
  (requires a-normal-form)   ; Just because I'm lazy
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
  ;; TODO Does it require unique-names?
  (requires syntax-checked a-normal-form)
  (generates aggregates-replaced)
  ;; Because of the reconstruction
  (sra-may-destroy aggregates-replaced)
  ;; Because of reconstruction and let-values simplification (?)
  (destroys lets-lifted)
  ;; By making aliases, and exposing structure slots to CSE
  (destroys no-common-subexpressions)
  ;; By exposing structure slots as variables
  (destroys no-intraprocedural-dead-variables
            no-interprocedural-dead-variables))

(define-stage intraprocedural-cse
  %intraprocedural-cse
  (requires syntax-checked)
  (requires unique-names) ; Because it does some internal let-lifting
  ;; These two requirements are not really requirements, but it works
  ;; much better this way.
  (requires a-normal-form lets-lifted)
  (generates no-common-subexpressions)
  ;; By leaving some dead aliases around
  (destroys no-intraprocedural-dead-variables
            no-interprocedural-dead-variables))

(define-stage eliminate-intraprocedural-dead-code
  %eliminate-intraprocedural-dead-code
  ;; Does not preserve fully-inlined in general because may alter the
  ;; call graph, but on all current examples it does.
  ;; When there are union types, it may destroy aggregates-replaced
  ;; for the same reason.
  (generates no-intraprocedural-dead-variables)
  ;; TODO Does it really require unique names?
  (requires syntax-checked unique-names))

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
  (generates no-interprocedural-dead-variables)
  ;; By running intraprocedural as a post-pass
  (generates no-intraprocedural-dead-variables))

(define-stage reverse-anf
  %reverse-anf
  ;; TODO Does it really preserve lets-lifted?
  (destroys a-normal-form)
  (requires syntax-checked unique-names))

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
    (lambda (program . extra)
      (display "(")
      (pp (stage-data-name stage-data))
      (begin1
       (apply exec program extra)
       (pp (hash-table/get eq-properties answer #f))
       (display ")")))))

(define (report-stage-progress stage name show-program)
  (lambda (program . extra)
    (format #t "Stage ~A on " name)
    (show-program program)
    (abegin1 (show-time (lambda () (apply stage program extra)))
      (newline)
      (when (eq? name 'generate)
        ;; TODO This was done only in visibly mode in the old world
        ;; order, presumably because it explicitly invokes the GC,
        ;; which would make the test suite too slow if it were done
        ;; after every code generation.
        ;; TODO I need a real module system!
        ((access clear-name-caches! user-initial-environment)))
      (when (eq? name 'reverse-anf)
        (display "Final output has ")
        (show-program it)))))

(define (visibly stage-data)
  (lambda (exec)
    (report-stage-progress exec (stage-data-name stage-data)
     (lambda (program)
       (if (eq? (stage-data-name stage-data) 'generate)
           ;; The generate stage wants to display different stats
           (format #t "~A bindings\n"
                   (length
                    ((access analysis-bindings user-initial-environment)
                     (property-value 'analysis program))))
           (print-fol-size program))))))

(define (volubly stage-data)
  (lambda (exec)
    (report-stage-progress exec (stage-data-name stage-data)
     (lambda (program)
       (if (eq? (stage-data-name stage-data) 'generate)
           ;; The generate stage wants to display different stats
           (format #t "analysis of size ~A"
                   (estimate-space-usage (property-value 'analysis program)))
           (print-fol-statistics program))))))

(define (optimize-visibly program)
  (fol-optimize program visibly))
