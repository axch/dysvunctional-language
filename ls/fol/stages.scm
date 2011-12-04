(declare (usual-integrations))

;;;; FOL Stage Manager

;;; See fol/doc/stage-manager.txt for motivation, overview, and basic
;;; usage instructions.  Read on for how it works and how to implement
;;; sophisticated extensions.

;;; In addition to the functions that define each primitive pass of
;;; the FOL compiler, we want to define and manipulate explicit stage
;;; objects that record information about the stages.  We also
;;; annotate the top node of a FOL program with various eq properties,
;;; which the stage management will inspect to decide what to do.

;;; The possible annotations are going to be markers about whether the
;;; program is known to have some property:
;;; - Structures replaced by vectors
;;; - Syntax checked and type correct
;;; - Type of the entry point
;;; - Names are unique
;;; - Approximate ANF
;;; - Lets Lifted
;;; - Maximally inlined
;;; - Aggregates have been replaced
;;; - Common subexpressions have been eliminated
;;; - Dead code has been eliminated intraprocedurally
;;; - Dead code has been eliminated interprocedurally
;;; - The flow analysis of a VL/DVL program is passed as a property too

;;; For each stage, I want to know the following information:
;;; - Does it create, preserve, or break each property (default preserve)?
;;;   (The fourth option is toggle, but that doesn't happen in this system)
;;; - Does it require each property?
;;; - check-program-types computes the type of the entry point
;;; - generate reads the analysis of its program

;;; Given this, we arrange for each stage to inspect the annotations
;;; and precede itself with whatever processing it requires if it
;;; hasn't been done yet.

;;; We also express the toplevel compiler as an explicit composition
;;; of stages, so that it can be automatically augmented to
;;; - Report per-stage statistics on input size, time, etc.
;;; - Check that all annotations are true
;;;   - This automates much of the invariant checking in
;;;     vl/test/utils.scm

;;; It should be possible to express the idea "Make this darn thing
;;; much smaller and then I might inline it some more" as an
;;; annotation, and write the loop that does inline-CSE-dead-code
;;; until convergence as a stage combinator.

;;;; Properties

;;; A property is just a marker (compared by eq?).  Programs may have
;;; properties with various values; also, properties can be generated,
;;; so we need to track of what can generate any particular property.

(define (property-value property program)
  (eq-get program property))

(define (property-value! property value program)
  (if value
      (eq-put! program property value)
      (eq-rem! program property))
  program)

(define (present? property program)
  (not (not (property-value property program))))

(define (present! property program)
  (property-value! property #t program))

(define (absent! property program)
  (property-value! property #f program))

(define *property-generators* '())

(define (lookup-generator property)
  (force-assq property *property-generators*))

(define (register-generator! property)
  (lambda (generator-data)
    (set! *property-generators*
          (cons (cons property (stage-data->stage generator-data))
                *property-generators*))))

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

;;; A stage also accepts any number of optional arguments for how to
;;; do it, wherewith it wraps all its constituent stages.
(define (execute-stage stage program . how)
  (if (null? how)
      ((stage-data->execution-function (entity-extra stage)) program)
      (apply execute-stage (do-substages stage (car how)) program (cdr how))))

(define (stage-name stage)
  (stage-data-name (entity-extra stage)))

;;; What information do we maintain about a stage?  There is the name
;;; and the actual execution procedure.  Stages may have dependencies
;;; on other stages, so we have an alist of the stages this one
;;; depends on, and a prepare procedure that accepts this alist and
;;; the input program, and produces a massaged input program suitable
;;; for the execution procedure.  This is separated out this way to
;;; allow recursive walks of the stage-data structures to modify all
;;; stages, including the dependencies of a given stage.  (Which would
;;; be impossible if those dependencies were hidden in a closure).

;;; Typologically, the dependencies slot is an a-list mapping the key
;;; by which this stage knows its dependency to the dependency in
;;; question (which should be a stage object).  The execution function
;;; may be #f to indicate no execution function; for example, a stage
;;; that is just a pipeline does all its work in the preparation step,
;;; and has no execution function of its own.

(define-structure
  (stage-data
   safe-accessors
   (constructor make-stage-data ())
   (constructor %make-stage-data (name prepare dependencies execute)))
  (name #f)
  (prepare (lambda (deps) (lambda (prog) prog)))
  (dependencies '())
  (execute #f))

(define (stage-data->execution-function stage-data)
  (lambda (program . extra)
    (let ((prepared
           (((stage-data-prepare stage-data)
             (stage-data-dependencies stage-data))
            program))
          (exec (stage-data-execute stage-data)))
      (if exec (apply exec prepared extra) prepared))))

;;; Given this setup, here is how to wrap all stages in a stage
;;; composition with the same wrapper.  The HOW argument is expected
;;; to be a procedure that acepts a stage data object and returns a
;;; procedure that accepts a stage execution function and returns a
;;; (new) stage execution function.  The reason for passing the stage
;;; data is that the wrapper may wish to read it.

(define (do-substages stage how)
  (let loop ((stage stage))
    (cond ((stage? stage)
           (stage-data->stage (loop (entity-extra stage))))
          ((stage-data? stage)
           (%make-stage-data
            (stage-data-name stage)
            (stage-data-prepare stage)
            (map loop (stage-data-dependencies stage))
            ;; Do not try to wrap nonexistent execution functions
            (fmap-maybe (how stage) (stage-data-execute stage))))
          ((pair? stage)
           ;; Alist entry
           (cons (car stage) (loop (cdr stage))))
          (else
           (error "Adjusting a non-stage" stage)))))

;;; A stage that is just a pipeline of other stages has a prepare
;;; operation that runs all of its dependencies, and does nothing in
;;; its execution step.

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
      #f))))

(define (loop-while-shrinks substage)
  (define (loop substage-alist)
    (let ((substage (cdar substage-alist)))
      (lambda (program)
        (pp `looping)
        (let repeat ((old-program program)
                     (old-size (count-pairs program)))
          (let* ((new-program (substage old-program))
                 (new-size (count-pairs new-program)))
            (pp `(deciding whether to repeat ,old-size ,new-size))
            (if (>= new-size old-size)
                new-program
                (repeat new-program new-size)))))))
  (let ((name (stage-name substage)))
    (stage-data->stage
     (%make-stage-data
      `(shrink-loop ,name)
      loop
      `((,name . ,substage))
      #f))))

;;; We define a nice language for specifying stages.  For example,
;;;
;;; (define-stage scalar-replace-aggregates
;;;   raw-scalar-replace-aggregates
;;;   (requires type-correct unique-names a-normal-form)
;;;   (preserves type-correct unique-names)
;;;   (destroys a-normal-form lets-lifted))
;;;
;;; should define a stage named scalar-replace-aggregates, implemented
;;; by the function raw-scalar-replace-aggregates, which would be
;;; known to require and preserve the indicated properties.
;;; Requirement of properties that are generated by other stages
;;; entails those other stages becoming dependencies of this one, but
;;; does not entail that they will always be run when this one is run
;;; (namely, if the property is already there, the process that
;;; produces it need not be repeated).

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
           ,(named-lambda (the-name . args)
              ;; Defeat of tail recursion shows up in the stack sampler.
              (begin1 (apply exec args))))
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
;;;      (,destroys a-normal-form lets-lifted))))

;;; Now, the stage parser's job is to construct the stage object
;;; implementing the stage from the clauses it is given.  It seemed
;;; easiest to represent each clause type as a function (parameterized
;;; by any clause arguments) that would mutate a partially-initialized
;;; stage-data object.  This way one can extend the set of possible
;;; clauses without changing the parser.  Note that the semantics of
;;; multiple clause arguments are the same as separating them out into
;;; several clauses of the same type.

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

;;; Now for the actual clause types.  There are two simple clauses
;;; that just set a field of the stage object:

(define (name the-name)
  (lambda (stage-data)
    (set-stage-data-name! stage-data the-name)))

(define (execution-function f)
  (lambda (stage-data)
    (set-stage-data-execute! stage-data (preserves-everything f))))

(define (preserves-everything exec)
  (define (fresh-cons-cell thing)
    (if (pair? thing)
        (cons (car thing) (cdr thing))
        thing))
  (lambda (program . extra)
    (abegin1
     ;; Fresh object prevents confusion of output's eq-properties with
     ;; input's eq-properties.
     (fresh-cons-cell (apply exec program extra))
     (eq-clone! program it))))

;;; The REQUIRES clause attaches a generator of the needed property as
;;; a dependency of this stage, and modifies the prepare of this stage
;;; to make it check whether the program actually has the given
;;; property, and pre-invoke the generator if it does not.  The fact
;;; that generators are looked up here means that stages have to be
;;; defined in dependency order.

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
  (both
   (modify-execution-function
    (lambda (exec)
      (lambda (program . extra)
        (present! property (apply exec program extra)))))
   (register-generator! property)))

(define (both f g)
  (lambda (x)
    (f x)
    (g x)))

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
     (lambda (program . extra)
       (let ((val (property-value property program)))
         (property-value! property val (apply exec program extra)))))))

;;; The DESTROYS clause modifies the execution function to flush the
;;; value of the given property.

(define (destroys property)
  (modify-execution-function
   (lambda (exec)
     (lambda (program . extra)
       (absent! property (apply exec program extra))))))

;;; The semantics of COMPUTES are that the execution function doesn't
;;; actually modify the program at all, it just computes the value that
;;; should be stored in the given property annotation.  This then
;;; implies preservation of all other properties.

(define (computes property)
  (both
   (modify-execution-function
    (lambda (exec)
      (lambda (program . extra)
        (let ((answer (apply exec program extra)))
          (property-value! property answer program)))))
   (register-generator! property)))

;;; The semantics of READS are that the underlying execution function
;;; accepts an additional argument besides the program (see GENERATE
;;; in vl/code-generator.scm) which should be fetched from the named
;;; property.

(define (reads property)
  (modify-execution-function
   (lambda (exec)
     (lambda (program . extra)
       (apply exec program (property-value property program) extra)))))
