(declare (usual-integrations))

;;; In addition to the functions that define each primitive pass of the
;;; FOL compiler, define explicit stage objects that record information
;;; about the stages.  Also assume that the top node of a FOL program may
;;; be annotated (with various eq properties).

;;; The possible annotations are going to be markers about whether the
;;; program is known to have some property:
;;; - Syntax checked and type correct
;;; - Names are unique
;;; - Approximate ANF
;;; - Lets Lifted (if I want this)
;;; - Maybe also each "stage done" property:
;;;   - Maximally inlined
;;;   - Aggregates have been replaced
;;;   - Common subexpressions have been eliminated
;;;   - Dead code has been eliminated intraprocedurally
;;;   - Dead code has been eliminated interprocedurally
 
;;; For each stage, I want to know the following information:
;;; - Does it create, preserve, or break each property (default preserve)?
;;;   (The fourth option is toggle, but that doesn't happen in this system)
;;; - Does it require each property?
;;; - Is it idempotent?
;;; - Maybe, for each property, does it do nothing if that property
;;;   is present (idempotence can be expressed this way)?

;;; Given this, it should be possible to abstract the idea that each stage
;;; will inspect the annotations and precede itself with whatever
;;; processing it requires if it hasn't been done yet.

;;; It should also be possible to express the toplevel compiler as
;;; an explicit combination (mostly composition) of stages, so that
;;; it can be automatically augmented to
;;; - Report per-stage statistics on input size, time, etc.
;;;   - This automates optimize-visibly
;;; - Check that all annotations are true
;;;   - This automates much of the invariant checking in vl/test/utils.scm

;;; When the smart inliner comes online, it should be possible to express
;;; the idea "Make this darn thing much smaller and then I might inline it
;;; some more" as an annotation, and write the loop that does
;;; inline-CSE-dead-code until convergence as a stage combinator.  Do I
;;; actually need an explicit annotation?

;;; The raw functions should be explicitly named, if nothing else because
;;; I want to be able to call them in unit tests without the sanitization.
;;; The stage objects should also be explicitly named, because I want to
;;; be able to combine them as data and export (suitable combinations) as
;;; things the user might do.  Perhaps each stage should be an entity that
;;; interprets its own data structure.

;;; One more thought: much of what vl/test/utils.scm tests is
;;; commutativity of various stages.  Is there a good way to express this
;;; in the data structures, so that the proper commutativity checks for
;;; any given stage ordering can be inferred automatically?  Do the
;;; commutativity checks even depend on the stage ordering?

(define (requires property)
  (lambda (stage)
    (lambda (program)
      (if (present? property program)
          (stage program)
          (stage ((property-generator property) program))))))

(define (preserves property)
  (lambda (stage)
    (lambda (program)
      (if (present? property program)
          (present! property (stage program))
          (stage program)))))

(define (generates property)
  (lambda (stage)
    (lambda (program)
      (present! property (stage program)))))

(define (breaks property)
  (lambda (stage) stage))

(define-stage scalar-replace-aggregates
  (requires type-correct unique-names a-normal-form)
  (preserves type-correct unique-names)
  (breaks a-normal-form lets-lifted)
  idempotent)
