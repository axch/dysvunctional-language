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
    ((visible-stage interprocedural-dead-code-elimination)
     ((visible-stage eliminate-intraprocedural-dead-variables)
      (intraprocedural-cse-visibly
       (scalar-replace-aggregates-visibly
        ((visible-stage inline)             ; includes ALPHA-RENAME
         program))))))))
