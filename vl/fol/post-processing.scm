(declare (usual-integrations))
;;;; Post processing

;;; The post-processing stage consists of several sub-stages.  They
;;; need to be done in order, but you can invoke any subsequence to
;;; see the effect of doing only that level of post-processing.
;;; We have:
;;; - STRUCTURE-DEFINITIONS->VECTORS
;;;   Replace DEFINE-STRUCTURE with explicit vectors.
;;; - INLINE
;;;   Inline non-recursive function definitions.
;;; - SCALAR-REPLACE-AGGREGATES
;;;   Replace aggregates with scalars at procedure boundaries.
;;;   This relies on argument-type annotations being emitted by the
;;;   code generator.
;;; - STRIP-ARGUMENT-TYPES
;;;   Remove argument-type annotations, if they have been emitted by
;;;   the code generator (because SCALAR-REPLACE-AGGREGATES is the
;;;   only thing that needs them).
;;; - TIDY
;;;   Clean up and optimize locally by term-rewriting.
(define (fol-optimize output)
  ((lambda (x) x) ; This makes the last stage show up in the stack sampler
   (strip-argument-types
    (tidy
     (intraprocedural-dead-variable-elimination
      (intraprocedural-de-alias
       (sra-program
        (sra-anf
         (alpha-rename
          (inline
           (structure-definitions->vectors
            output)))))))))))

(define (compile-to-scheme program)
  (fol-optimize
   (analyze-and-generate program)))

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

(define empty-let-rule (rule `(let () (? body)) body))

;; This is safe assuming the program has been alpha renamed
(define let-let-lifting-rule
  (rule `(let ((?? bindings1)
               ((? name ,symbol?) (let (? in-bindings) (? exp)))
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
               ((? name ,symbol?) (? exp))
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
    (rule `(begin (? body)) body)
    (rule `(* 0 (? thing)) 0)
    (rule `(* (? thing) 0) 0)
    (rule `(+ 0 (? thing)) thing)
    (rule `(+ (? thing) 0) thing)
    (rule `(* 1 (? thing)) thing)
    (rule `(* (? thing) 1) thing)

    (rule `(if (? predicate) (? exp) (? exp))
          exp)

    (rule `(let-values (((? names) (values (?? stuff))))
             (?? body))
          `(let ,(map list names stuff)
             ,@body))

    empty-let-rule

    values-let-lifting-rule
    singleton-inlining-rule)))


