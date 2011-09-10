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

;;; A property is just a marker (compared by eq?).  Programs may have
;;; properties with various values; also, properties can be generated,
;;; so we need to track what can generate any particular property.

(define (property-value property program)
  (eq-get program property))

(define (property-value! property value program)
  (eq-put! program property value)
  program)

(define (present? property program)
  (not (not (property-value property program))))

(define (present! property program)
  (property-value! property #t program))

(define *property-generators* '())

(define (lookup-generator property)
  (force-assq property *property-generators*))

(define (register-generator! property generator)
  (set! *property-generators*
        (cons (cons property generator)
              *property-generators*)))

;;;; Stages

;;; A stage is an executable program transformation about which we
;;; have some information.  In typology, a stage is an entity whose
;;; entity-extra is the information we know about that stage, and
;;; which can be applied to a program to do whatever operation that
;;; stage does.

(define (stage? thing)
  (and (entity? thing) (stage-data? (entity-extra thing))))

(define (stage-data->stage stage-data)
  (make-entity execute-stage stage-data))

(define (execute-stage stage program)
  ((stage-data->execution-function (entity-extra stage)) program))

(define (stage-name stage)
  (stage-data-name (entity-extra stage)))

;;; What information do we maintain about a stage?  There is the name,
;;; and a flag about whether or not this stage is known to be
;;; idempotent.  In order to be able to adjust all the stages involved
;;; in some process, we also maintain the other stages this one may
;;; depend on in the dependencies slot.  This slot is an a-list
;;; mapping the key by which this stage knows its dependency to the
;;; dependency in question (which should be a stage object, or just a
;;; function).  In order for these adjustments to have any effect, the
;;; action of the stage is represented not by a procedure that does
;;; it, but by a combinator procedure that accepts the dependency
;;; alist and computes the execution function from it.  An execution
;;; function is a function that accepts a program and does the work of
;;; the stage.

(define-structure
  (stage-data
   safe-accessors
   (constructor make-stage-data ())
   (constructor %make-stage-data
                (name prepare dependencies execute idempotent?)))
  (name #f)
  (prepare (lambda (deps) (lambda (prog) prog)))
  (dependencies '())
  (execute #f)
  (idempotent? #f))

(define (stage-data->execution-function stage-data)
  (lambda (program)
    ((stage-data-execute stage-data)
     (((stage-data-prepare stage-data)
       (stage-data-dependencies stage-data))
      program))))

;;; Given this setup, here is how to wrap all stages in a stage
;;; composition with the same wrapper.

(define (do-stages stage how)
  (let loop ((stage stage))
    (cond ((stage? stage)
           (stage-data->stage (loop (entity-extra stage))))
          ((stage-data? stage)
           (%make-stage-data
            (stage-data-name stage)
            (stage-data-prepare stage)
            (map loop (stage-data-dependencies stage))
            (fmap-maybe (how stage) (stage-data-execute stage))
            ;; Wrapping dependencies preserves idempotence
            (stage-data-idempotent? stage)))
          ((pair? stage)
           ;; Alist entry
           (cons (car stage) (loop (cdr stage))))
          (else
           (error "Adjusting a non-stage" stage)))))

;;; The definition of a stage that is just a pipeline of other stages
;;; is easy too.

(define (stage-pipeline . substages)
  (define (compose substage-alist)
    (let loop ((fs (map cdr substage-alist)))
      (if (null? fs)
          (lambda (x) x)
          (lambda (x)
            ((car fs) ((loop (cdr fs)) x))))))
  (let ((names (map stage-name substages)))
    (stage-data->stage
     (%make-stage-data
      `(composition ,@names)
      compose
      (map cons names substages)
      #f
      #f))))

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
;;; properties.  Requirement of properties that are generated by other
;;; stages entails those other stages becoming dependencies of this
;;; one, but does not entail that they will always be run when this
;;; one is run (namely, if the property is already there, the process
;;; that produces it need not be repeated).

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

;;; Now, the stage parser's job is to construct the stage object
;;; implementing the stage from the clauses it is given.  It seemed
;;; easiest to represent each clause type as a function (parameterized
;;; by any clause arguments) that would mutate a partially-initialized
;;; stage-data object.  This way I can extend the set of possible
;;; clauses without changing the parser.  Note that the semantics of
;;; multiple clause arguments are the same as separating them out into
;;; several clauses of the same type.  Note also that clause
;;; specifications do not operationally commute.

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

;;; Now for the actual clause types.  There are three simple clauses
;;; that just set a field of the stage object:

(define (name the-name)
  (lambda (stage-data)
    (set-stage-data-name! stage-data the-name)))

(define (idempotent stage-data)
  (set-stage-data-idempotent?! stage-data #t))

(define (execution-function f)
  (lambda (stage-data)
    (set-stage-data-execute! stage-data f)))

;;; The REQUIRES clause attaches a generator of the needed property as
;;; a dependency of this stage, and modifies the prepare of this
;;; stage to make the execution function check whether the program
;;; actually has the given property, and pre-invoke the generator if
;;; it does not.

(define (requires property)
  (lambda (stage-data)
    (attach-dependency! stage-data property
     (lookup-generator property))
    (set-stage-data-prepare! stage-data
     (let ((preparer (stage-data-prepare stage-data)))
       (lambda (dependencies)
         (let ((prep (preparer dependencies)))
           (lambda (program)
             (if (present? property program)
                 (prep program)
                 (prep ((force-assq property dependencies)
                        program))))))))))

(define (attach-dependency! stage-data name object)
  (set-stage-data-dependencies! stage-data
   (cons (cons name object)
         (stage-data-dependencies stage-data))))

;;; The GENERATES clause modifies the execution function to tag the
;;; outgoing program as having the property, and registers the
;;; property as being generated by this stage.

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
    (set-stage-data-execute!
     stage-data
     (f (stage-data-execute stage-data)))))

;;; The PRESERVES clause just modifies the execution function to
;;; forward the value of the given property from the input to the
;;; output.

(define (preserves property)
  (modify-execution-function
   (lambda (exec)
     (lambda (program)
       (let ((val (property-value property program)))
         (property-value! property val (exec program)))))))

;;; The DESTROYS clause doesn't actually need to do anything, because
;;; not forwarding property annotations is the default.

(define (destroys property)
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

