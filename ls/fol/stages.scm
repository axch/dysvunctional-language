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


;;;; Properties

(define (property-value property program)
  (eq-get program property))

(define (property-value! property value program)
  (eq-put! program property value)
  program)

(define (present? property program)
  (not (not (property-value property program))))

(define (present! property program)
  (property-value! property #t program))

;;;; Stages

(define-structure
  (stage-data
   safe-accessors
   (constructor make-stage-data ())
   (constructor %make-stage-data (name combinator dependencies idempotent?)))
  (name #f)
  (combinator #f)
  (dependencies '())
  (idempotent? #f))

(define (stage-execution-function stage)
  (let ((stage-data (entity-extra stage)))
    ((stage-data-combinator stage-data)
     (stage-data-dependencies stage-data))))

(define (execute-stage stage program)
  ((stage-execution-function stage) program))

(define (stage? thing)
  (and (entity? thing) (stage-data? (entity-extra thing))))

(define stage-data entity-extra)

(define (stage-data->stage stage-data)
  (make-entity execute-stage stage-data))

;;; I want a nice language for specifying stages.  For example,
;;;
;;; (define-stage scalar-replace-aggregates
;;;   raw-scalar-replace-aggregates
;;;   (requires type-correct unique-names a-normal-form)
;;;   (preserves type-correct unique-names)
;;;   (destroys a-normal-form lets-lifted)
;;;   (idempotent))
;;;
;;; should define a stage named scalar-replace-aggregates, implemented
;;; by the function raw-scalar-replace-aggregates, which would be
;;; known to be idempotent and to require and preserve the indicated
;;; properties.

;;; The first step for a nice language is a macro to perform the
;;; desired syntactic abstraction.

(define-syntax define-stage
  (syntax-rules ()
    ((_ the-name exec (clause-head clause-arg ...) ...)
     (define the-name
       (parse-stage
        `((,name the-name)
          (,execution-function
           ;; Layer of indirection sees changes zapped at the REPL.
           ,(lambda (program) (exec program)))
          (,clause-head clause-arg ...) ...))))))

;;; The above example expands into
;;;
;;; (define scalar-replace-aggregates
;;;   (parse-stage
;;;    `((,name scalar-replace-aggregates)
;;;      (,execution-function
;;;       ,(lambda (program) (raw-scalar-replace-aggregates program)))
;;;      (,requires type-correct unique-names a-normal-form)
;;;      (,preserves type-correct unique-names)
;;;      (,destroys a-normal-form lets-lifted)
;;;      (,idempotent))))

;;; Now, the stage parser's job is to construct the data object
;;; representing the stage from the clauses it is given.  It seemed
;;; easiest to represent each clause type as a function (parameterized
;;; by any clause arguments) that would mutate a partially-initialized
;;; stage object.  This way I can extend the set of possible clauses
;;; without changing the parser.  Note that the semantics of multiple
;;; clause arguments are the same as separating them out into several
;;; clauses of the same type.

(define (parse-stage features)
  (define stage-data (make-stage-data))
  (for-each
   (lambda (clause)
     (if (not (null? (cdr clause)))
         (for-each (lambda (adjust!)
                     (adjust! stage-data))
                   (map (car clause) (cdr clause)))
         ((car clause) stage-data)))
   features)
  (stage-data->stage stage-data))

;;; Now for the actual clause types.  There are simple clauses that
;;; just set a field of the stage object:

(define (name the-name)
  (lambda (stage-data)
    (set-stage-data-name! stage-data the-name)))

(define (execution-function f)
  (lambda (stage-data)
    (set-stage-data-combinator!
     stage-data
     (lambda (dependencies)
       (force-assq 'base dependencies)))
    (attach-dependency! stage-data 'base f)))

(define (idempotent stage-data)
  (set-stage-data-idempotent?! stage-data #t))

(define (attach-dependency! stage-data name object)
  (set-stage-data-dependencies! stage-data
   (cons (cons name object)
         (stage-data-dependencies stage-data))))

;;; In addition, there are clauses that modify the execution function
;;; (once it's been set) by wrapping it in some wrapper that handles
;;; some property annotation.

(define (requires property)
  (lambda (stage-data)
    (set-stage-data-combinator! stage-data
     (lambda (dependencies)
       (lambda (program)
         (if (present? property program)
             (exec program)
             (exec ((force-assq property dependencies)
                    program))))))
    (attach-dependency! stage-data property
     (lookup-generator property))))

(define (generates property)
  (lambda (stage-data)
    ((modify-execution-function
      (lambda (exec)
        (lambda (program)
          (present! property (exec program)))))
     stage-data)
    (register-generator! property
     (stage-data->stage stage-data))))

(define (modify-execution-function f)
  (lambda (stage-data)
    (let ((combinator (stage-data-combinator stage-data)))
      (set-stage-data-combinator!
       stage-data
       (lambda (dependencies)
         (f (combinator dependencies)))))))

(define (preserves property)
  (modify-execution-function
   (lambda (exec)
     (lambda (program)
       (let ((val (property-value property program)))
         (property-value! property val (exec program)))))))

(define (destroys property)
  ;; Breaking is the effective default in the underlying implementation
  (lambda (stage) stage))

;; The semantics of COMPUTES are that the execution function doesn't
;; actually modify the program at all, it just computes the value that
;; should be stored in the given property annotation.  This then
;; implies preservation of all other properties.
(define (computes property)
  (lambda (stage-data)
    ((modify-execution-function
      (lambda (exec)
        (lambda (program)
          (let ((answer (exec program)))
            (property-value! property answer program)))))
     stage-data)
    (register-generator! property
     (stage-data->stage stage-data))))

;;; I want to be able to uniformly change stage pipelines by mapping
;;; some wrapper over all the primitive stages in the pipeline.

(define (do-stages stage how)
  (let loop ((stage stage))
    (cond ((pair? stage)
           ;; alist entry
           (cons (car stage) (loop (cdr stage))))
          ((stage? stage)
           (make-entity execute-stage
                        (loop (stage-data stage))))
          ((stage-data? stage)
           (how
            (%make-stage-data
             (stage-data-name stage)
             (stage-data-combinator stage)
             (map loop (stage-data-dependencies stage))
             ;; Wrapping dependencies preserves idempotence
             (stage-data-idempotent? stage)))))))

(define (stage-pipeline . substages)
  (define (compose . fs)
    (if (null? fs)
        (lambda (x) x)
        (lambda (x)
          ((car fs) ((compose (cdr fs)) x)))))
  (%make-stage-data 'composition compose substages #f))
