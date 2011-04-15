(declare (usual-integrations))
;;;; Dead code elimination

;;; Variables that hold values that are never used can be eliminated.
;;; The code that computes those values can also be eliminated.

;;; I only intraprocedural dead variable elimination.  See the end of
;;; the file for an outline of interprocedural dead variable
;;; elimination; that will be the reason why I don't do it (yet).

;;; Intraprocedural dead variable elimination can be done by recursive
;;; traversal of the code, traversing LET bodies before LET bindings.
;;; In the absence of multiple value returns, the recursion need not
;;; carry any information down, because the mere fact of being
;;; considered would mean that any given expression were live.  In the
;;; presence of multiple value returns, however, some of the returned
;;; values might be live while others are dead.  Therefore, the
;;; recursion carries down a list indicating which of the values that
;;; are expected to come out of the subexpression are live and which
;;; are not.  The recursive call must return both the subexpression
;;; with dead code removed, and the set of variables it uses to
;;; compute its live outputs.  Variables not in that set at their
;;; binding site are dead and may be removed.

;;; In this scheme, variables use themselves and constants use
;;; nothing.  If any of the outputs of an IF form are live, then its
;;; predicate is live, and the IF form uses everything its predicate
;;; and its branches use.  A LET body is processed first; any
;;; variables the LET binds that its body doesn't use are dead and can
;;; be skipped.  A LET-VALUES is analagous, except that some of the
;;; values may be live and others not.  In that case, the LET-VALUES
;;; drops the dead variables, and recurs on the subexpression giving
;;; it that mask for what to keep and what to leave off.  A VALUES
;;; form must conversely interpret the incoming mask and only keep
;;; those of its subexpressions that are actually needed.

;;; In principle, similar masking could be used to do elimination on
;;; the slots of structures, but for simplicity I have chosen instead
;;; to rely on SRA to separate said structures into individually named
;;; variables and just do elimination on those variables.  I may
;;; revisit this decision when I add union types.

;;; Procedure applications introduce a wrinkle.  Since the analysis is
;;; intraprocedural, is assumes that all the arguments of a live
;;; procedure call are live.  Furthermore, said procedure call may
;;; return multiple values, not all of which may be wanted by the
;;; caller.  The transformation cannot change the set of values the
;;; procedure will return, but the caller will already have been
;;; tranformed expecting to only be given live values.  At this point,
;;; the transformation pulls a trick: it binds all the values that the
;;; procedure will emit in a LET-VALUES, and immediately returns the
;;; live ones with a VALUES.  The others are bound to a special name
;;; that any future dead variable transform will recognize as "Yes, I
;;; know this variable is dead, but I have to bind it anyway because
;;; it's coming here whether I want it or not."  Analagously,
;;; procedure formal parameters are not removed even if they may be
;;; dead in the body.

(define (eliminate-intraprocedural-dead-variables program)
  (define eliminate-in-definition
    (rule `(define ((? name ,fol-var?) (?? formals))
             (argument-types (?? stuff) (? return))
             (? body))
          `(define (,name ,@formals)
             (argument-types ,@stuff ,return)
             ,(eliminate-in-expression
               body (or (not (values-form? return))
                        (map (lambda (x) #t) (cdr return)))))))
  (if (begin-form? program)
      (append
       (map eliminate-in-definition (except-last-pair program))
       (list (eliminate-in-expression
              (last program) #t)))
      (eliminate-in-expression program #t)))

(define (eliminate-in-expression expr live-out)
  (define (ignore? name)
    (eq? (name-base name) '_))
  ;; The live-out parameter indicates which of the return values of
  ;; this expression are needed by the context in whose tail position
  ;; this expression is evaluated.  It will be #t unless the context
  ;; is a LET-VALUES, in which case it will indicate which of those
  ;; multiple values are needed.  If I were eliminating dead structure
  ;; slots as well, this would be hairier.
  ;; This is written in continuation passing style because the
  ;; recursive call needs to return two things.  The win continuation
  ;; accepts the transformed expression and the set of variables that
  ;; it needs to compute its live results.
  (define (loop expr live-out win)
    (cond ((fol-var? expr)
           (win expr (single-used-var expr)))
          ((number? expr)
           (win expr (no-used-vars)))
          ((boolean? expr)
           (win expr (no-used-vars)))
          ((null? expr)
           (win expr (no-used-vars)))
          ((if-form? expr)
           (eliminate-in-if expr live-out win))
          ((let-form? expr)
           (eliminate-in-let expr live-out win))
          ((let-values-form? expr)
           (eliminate-in-let-values expr live-out win))
          ;; If used post SRA, there may be constructions to build the
          ;; answer for the outside world, but there should be no
          ;; accesses.
          ((construction? expr)
           (eliminate-in-construction expr live-out win))
          ((access? expr)
           (eliminate-in-access expr live-out win))
          ((values-form? expr)
           (eliminate-in-values expr live-out win))
          (else ; general application
           (eliminate-in-application expr live-out win))))
  (define (eliminate-in-if expr live-out win)
    (let ((predicate (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
      (loop predicate #t
       (lambda (new-predicate pred-used)
         (loop consequent live-out
          (lambda (new-consequent cons-used)
            (loop alternate live-out
             (lambda (new-alternate alt-used)
               (win `(if ,new-predicate
                         ,new-consequent
                         ,new-alternate)
                    (var-set-union
                     pred-used (var-set-union cons-used alt-used)))))))))))
  (define (eliminate-in-let expr live-out win)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (loop body live-out
       (lambda (new-body body-used)
         (let ((new-bindings
                (filter (lambda (binding)
                          (var-used? (car binding) body-used))
                        bindings)))
           (loop* (map cadr new-bindings)
            (lambda (new-exprs exprs-used)
              (let ((used (var-set-union* exprs-used)))
                (win (empty-let-rule
                      `(let ,(map list (map car new-bindings)
                                  new-exprs)
                         ,new-body))
                     (var-set-union
                      used (var-set-difference
                            body-used (map car bindings))))))))))))
  (define (eliminate-in-let-values expr live-out win)
    (let ((binding (caadr expr))
          (body (caddr expr)))
      (let ((names (car binding))
            (sub-expr (cadr binding)))
        (loop body live-out
         (lambda (new-body body-used)
           (define (slot-used? name)
             (or (ignore? name)
                 (and (var-used? name body-used)
                      #t)))
           (let ((sub-expr-live-out (map slot-used? names)))
             (if (any (lambda (x) x) sub-expr-live-out)
                 (loop sub-expr sub-expr-live-out
                  (lambda (new-sub-expr sub-expr-used)
                    (win (tidy-let-values
                          `(let-values ((,(filter slot-used? names)
                                         ,new-sub-expr))
                             ,new-body))
                         (var-set-union
                          sub-expr-used
                          (var-set-difference body-used names)))))
                 (win new-body body-used))))))))
  ;; Given that I decided not to do proper elimination of dead
  ;; structure slots, I will say that if a structure is needed then
  ;; all of its slots are needed, and any access to any slot of a
  ;; structure causes the entire structure to be needed.
  (define (eliminate-in-construction expr live-out win)
    (loop* (cdr expr)
     (lambda (new-args args-used)
       (win `(,(car expr) ,@new-args)
            (var-set-union* args-used)))))
  (define (eliminate-in-access expr live-out win)
    (loop (cadr expr) #t
     (lambda (new-accessee accessee-uses)
       (win `(,(car expr) ,new-accessee ,@(cddr expr))
            accessee-uses))))
  (define (eliminate-in-values expr live-out win)
    (assert (list? live-out))
    (let ((wanted-elts (filter-map (lambda (wanted? elt)
                                     (and wanted? elt))
                                   live-out
                                   (cdr expr))))
      (loop* wanted-elts
       (lambda (new-elts elts-used)
         (win (if (= 1 (length new-elts))
                  (car new-elts)
                  `(values ,@new-elts))
              (var-set-union* elts-used))))))
  (define (eliminate-in-application expr live-out win)
    (loop* (cdr expr)
     (lambda (new-args args-used)
       (define (all-wanted? live-out)
         (or (equal? live-out #t)
             (every (lambda (x) x) live-out)))
       (define (invent-name wanted?)
         (if wanted?
             (make-name 'receipt)
             (make-name '_)))
       (let ((simple-new-call `(,(car expr) ,@new-args)))
         (let ((new-call
                (if (all-wanted? live-out)
                    simple-new-call
                    (let* ((receipt-names (map invent-name live-out))
                           (useful-names (filter (lambda (x) (not (ignore? x)))
                                                 receipt-names)))
                      `(let-values ((,receipt-names ,simple-new-call))
                         ,(if (> (length useful-names) 1)
                              `(values ,@useful-names)
                              (car useful-names)))))))
           (win new-call (var-set-union* args-used)))))))
  (define (loop* exprs win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) #t
         (lambda (new-expr expr-used)
           (loop* (cdr exprs)
            (lambda (new-exprs exprs-used)
              (win (cons new-expr new-exprs)
                   (cons expr-used exprs-used))))))))
  (loop expr live-out (lambda (new-expr used-vars) new-expr)))

(define (no-used-vars) '())

(define (single-used-var var) (list var))

(define (var-set-union vars1 vars2)
  (lset-union eq? vars1 vars2))

(define (var-set-union* vars-lst)
  (reduce var-set-union (no-used-vars) vars-lst))

(define (var-set-difference vars1 vars2)
  (lset-difference eq? vars1 vars2))

(define (var-set-union-map f vars)
  (var-set-union* (var-set-map f vars)))

(define (var-set-map f vars)
  (map f vars))

(define var-used? memq)

(define var-set-size length)

;;; To do interprocedural dead variable elimination I have to proceed
;;; as follows:
;;; -1) Run a round of intraprocedural dead variable elimination to
;;;     diminish the amount of work in the following (assume all
;;;     procedure calls need all their inputs)
;;; 0) Treat the final expression as a nullary procedure definition
;;; 1) Initialize a map for each procedure, mapping from output that
;;;    might be desired (by index) to input that is known to be needed
;;;    to compute that output.
;;;    - I know the answer for primitives
;;;    - All compound procedures start mapping every output to the empty
;;;      set of inputs known to be needed.
;;; 2) I can improve the map by walking the body of a procedure, carrying
;;;    down the set of desired outputs and bringing up the map saying
;;;    which outputs require which inputs
;;;    - Start with all outputs desired.
;;;    - A number requires no inputs for one output
;;;    - A variable requires itself for one output
;;;    - A VALUES maps its subexpressions to the desired outputs
;;;    - A LET is transparent on the way down, but if the variable it
;;;      is binding is desired as an input to its body, it recurs on
;;;      its expression desiring the one output.  Whatever inputs come
;;;      up need to be spliced in to the answers in the map coming from
;;;      the body.
;;;    - A LET-VALUES is analagous, but may choose to desire a subset
;;;      of its bound names.
;;;    - An IF recurs on the predicate desiring its output, and then
;;;      on the consequent and alternate passing the requests.  When
;;;      the answers come back, it needs to union the consequent and
;;;      alternate maps, and then add the predicate requirements as
;;;      inputs to all desired outputs of the IF.
;;;    - A procedure call refers to the currently known map for that
;;;      procedure.
;;;    - Whatever comes out of the top becomes the new map for this
;;;      procedure.
;;; 3) Repeat step 2 until no more improvements are possible.
;;; 4) Initialize a table of which inputs and outputs to each compound
;;;    procedure are actually needed.
;;;    - All procedures start not needed
;;;    - The entry point starts fully needed
;;; 5) I can improve this table by walking the body of a procedure
;;;    some of whose outputs are needed, carrying down the set of outputs
;;;    that are needed and bringing back up the set of inputs that are needed.
;;;    - At a procedure call, mark outputs of that procedure as needed in
;;;      the table if I found that I needed them on the walk; then take back
;;;      up the set of things that that procedure says it needs.
;;;    - Otherwise walk as in step 2 (check this!)
;;; 6) Repeat step 5 until no more improvements are possible.
;;; 7) Replace all definitions to
;;;    - Accept only those arguments they need (internally LET-bind all
;;;      others to tombstones)
;;;    - Return only those outputs that are needed (internally
;;;      LET-VALUES everything the body will generate, and VALUES out
;;;      that which is wanted)
;;; 8) Replace all call sites to
;;;    - Supply only those arguments that are needed (just drop
;;;      the rest)
;;;    - Solicit only those outputs that are needed (LET-VALUES them,
;;;      and VALUES what the body expects, filling in with tombstones).
;;; 9) Run a round of intraprocedural dead variable elimination to
;;;    clean up (all procedure calls now do need all their inputs)
;;;    - Verify that all the tombstones vanish.

(define (program->procedure-definitions program)
  (define (expression->procedure-definition entry-point return-type)
    `(define (%%main)
       (argument-types ,return-type) ;; TODO Get the actual type of the entry point
       ,entry-point))
  (let ((return-type (check-program-types program)))
    (if (begin-form? program)
        (append (cdr (except-last-pair program))
                (list (expression->procedure-definition (last program) return-type)))
        (list (expression->procedure-definition program return-type)))))

(define (procedure-definitions->program defns)
  (tidy-begin
   `(begin
      ,@(except-last-pair defns)
      ,((rule `(define (%%main)
                 (argument-types (? whatever))
                 (? expr))
              expr)
        (last defns)))))

(define (interprocedural-dead-code-elimination program)
  (let* ((defns (program->procedure-definitions program))
         (i/o-need-map (compute-i/o-need-map defns))
         (needed-var-map ((compute-need-map i/o-need-map) defns))
         (rewritten (rewrite-definitions i/o-need-map needed-var-map defns)))
    (eliminate-intraprocedural-dead-variables ;; TODO Check absence of tombstones
     (procedure-definitions->program
      rewritten))))

;;; The i/o-need-map is the structure built by steps 1-3 above.  It
;;; maps every procedure name to a list of sets of numbers.  The list
;;; is parallel to the values that the procedure returns, and each set
;;; of numbers indicates which of the procedure's inputs are needed
;;; for it to compute that output.

(define (initial-i/o-need-map defns)
  (define (primitive-i/o-need-map)
   (define (nullary name)
     (cons name (list (no-used-vars))))
   (define (unary name)
     (cons name (list (single-used-var 0))))
   (define (binary name)
     (cons name (list (var-set-union (single-used-var 0) (single-used-var 1)))))
   (alist->eq-hash-table
    `(,@(map nullary '(read-real gensym))
      ;; Type testers real? gensym? null? pair? should never be emitted
      ,@(map unary '(abs exp log sin cos tan asin acos sqrt write-real real
                         zero? positive? negative?))
      ,@(map binary '(+ - * / atan expt < <= > >= = gensym=)))))
  (let ((answer (primitive-i/o-need-map)))
    (for-each
     (rule `(define ((? name) (?? args))
              (argument-types (?? stuff) (? return))
              (? body))
           (hash-table/put! answer name
            (map (lambda (item) (no-used-vars))
                 (desirable-slot-list return))))
     defns)
    answer))

(define (improve-i/o-need-map defn i/o-need-map)
  (define (loop expr live-out)
    (cond ((fol-var? expr)
           (list (single-used-var expr)))
          ((number? expr)
           (list (no-used-vars)))
          ((boolean? expr)
           (list (no-used-vars)))
          ((null? expr)
           (list (no-used-vars)))
          ((if-form? expr)
           (study-if expr live-out))
          ((let-form? expr)
           (study-let expr live-out))
          ((let-values-form? expr)
           (study-let-values expr live-out))
          ;; If used post SRA, there may be constructions to build the
          ;; answer for the outside world, but there should be no
          ;; accesses.
          ((construction? expr)
           (study-construction expr live-out))
          ((access? expr)
           (study-access expr live-out))
          ((values-form? expr)
           (study-values expr live-out))
          (else ; general application
           (study-application expr live-out))))
  (define (study-if expr live-out)
    (let ((predicate (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
      (let ((pred-needs (car (loop predicate (list #t))))
            (cons-needs (loop consequent live-out))
            (alt-needs (loop alternate live-out)))
        (map
         (lambda (live? needed-in)
           (if live?
               (var-set-union pred-needs needed-in)
               (no-used-vars)))
         live-out
         (map var-set-union cons-needs alt-needs)))))
  (define (study-let expr live-out)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (let ((body-needs (loop body live-out))
            (bindings-need (map (lambda (binding)
                                  (cons (car binding)
                                        (car (loop (cadr binding) (list #t)))))
                                bindings)))
        (map
         (lambda (live? needs)
           (if live?
               (var-set-union-map
                (lambda (needed-var)
                  (let ((xxx (assq needed-var bindings-need)))
                    (if xxx
                        (cdr xxx)
                        (single-used-var needed-var))))
                needs)
               (no-used-vars)))
         live-out
         body-needs))))
  (define (study-let-values expr live-out)
    (let* ((binding (caadr expr))
           (names (car binding))
           (sub-expr (cadr binding))
           (body (caddr expr)))
      (let ((body-needs (loop body live-out))
            (bindings-need
             (map cons names (loop sub-expr (map (lambda (x) #t) names)))))
        (map
         (lambda (live? needs)
           (if live?
               (var-set-union-map
                (lambda (needed-var)
                  (let ((xxx (assq needed-var bindings-need)))
                    (if xxx
                        (cdr xxx)
                        (single-used-var needed-var))))
                needs)
               (no-used-vars)))
         live-out
         body-needs))))
  (define (study-construction expr live-out)
    (list
     (var-set-union*
      (map (lambda (arg)
             (car (loop arg (list #t))))
           (cdr expr)))))
  (define (study-access expr live-out)
    (loop (cadr expr) (list #t)))
  (define (study-values expr live-out)
    (map
     (lambda (live? sub-expr)
       (if live?
           (car (loop sub-expr (list #t)))
           (no-used-vars)))
     live-out
     (cdr expr)))
  (define (study-application expr live-out)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (let ((operator-i/o-need-map (hash-table/get i/o-need-map operator #f))
            (operands-need (map (lambda (operand)
                                  (car (loop operand (list #t))))
                                operands)))
        (map
         (lambda (live? operator-i/o-need)
           (if live?
               (var-set-union-map
                (lambda (needed-index)
                  (list-ref operands-need needed-index))
                operator-i/o-need)
               (no-used-vars)))
         live-out
         operator-i/o-need-map))))
  (define improve-i/o-need-map
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          (map
           (lambda (out-needs)
             (var-set-map (lambda (var)
                            (list-index (lambda (arg) (eq? var arg)) args))
                          out-needs))
           (loop body (map (lambda (x) #t) (desirable-slot-list return))))))
  (improve-i/o-need-map defn))

(define (desirable-slot-list shape)
  (if (values-form? shape)
      (cdr shape)
      (list shape)))

(define ((iterate-defn-map initialize improve-locally) defns)
  (let loop ((overall-map (initialize defns))
             (maybe-done? #t))
    (for-each
     (lambda (defn)
       (let ((local-map (improve-locally defn overall-map)))
         (if (equal? local-map (hash-table/get overall-map (definiendum defn) #f))
             'ok
             (begin
               (hash-table/put! overall-map (definiendum defn) local-map)
               (set! maybe-done? #f)))))
     defns)
    (if (not maybe-done?)
        (loop overall-map #t)
        overall-map)))

(define compute-i/o-need-map
  (iterate-defn-map initial-i/o-need-map improve-i/o-need-map))

(define ((compute-need-map i/o-need-map) defns)
  (let ((need-map (initial-need-map defns)))
    (let loop ()
      (clear-changed! need-map)
      (for-each
       (lambda (defn)
         ((improve-need-map! i/o-need-map) defn need-map))
       defns)
      (if (changed? need-map)
          (loop)
          need-map))))

(define (clear-changed! thing)
  (eq-put! thing 'changed #f))

(define (changed? thing)
  (eq-get thing 'changed))

(define (changed! thing)
  (eq-put! thing 'changed #t))

;;; The need map is the structure constructed during steps 4-6 above.
;;; It maps every procedure name to the set of its outputs that are
;;; actually needed.  The needed inputs can be inferred from this
;;; given the i/o-need-map.

(define (initial-need-map defns)
  (let ((answer
         (alist->eq-hash-table
          (map (rule `(define ((? name) (?? args))
                        (argument-types (?? stuff) (? return))
                        (? body))
                     (cons name
                           (map (lambda (x) #f) (desirable-slot-list return))))
               defns))))
    (hash-table/put! answer (definiendum (last defns)) (list #t))
    answer))

;;; TODO This file now contains *three* very similar recursive traversals!
(define ((improve-need-map! i/o-need-map) defn need-map)
  (define (loop expr live-out)
    (cond ((fol-var? expr)
           (single-used-var expr))
          ((number? expr)
           (no-used-vars))
          ((boolean? expr)
           (no-used-vars))
          ((null? expr)
           (no-used-vars))
          ((if-form? expr)
           (study-if expr live-out))
          ((let-form? expr)
           (study-let expr live-out))
          ((let-values-form? expr)
           (study-let-values expr live-out))
          ;; If used post SRA, there may be constructions to build the
          ;; answer for the outside world, but there should be no
          ;; accesses.
          ((construction? expr)
           (study-construction expr live-out))
          ((access? expr)
           (study-access expr live-out))
          ((values-form? expr)
           (study-values expr live-out))
          (else ; general application
           (study-application expr live-out))))
  (define (study-if expr live-out)
    (let ((predicate (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
      (let ((pred-needs (loop predicate (list #t)))
            (cons-needs (loop consequent live-out))
            (alt-needs (loop alternate live-out)))
        (var-set-union pred-needs (var-set-union cons-needs alt-needs)))))
  (define (study-let expr live-out)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (let* ((body-needs (loop body live-out))
             (bindings-need
              (map (lambda (binding)
                     (cons (car binding)
                           (if (var-used? (car binding) body-needs)
                               (loop (cadr binding) (list #t))
                               (no-used-vars))))
                   bindings)))
        (var-set-union-map
         (lambda (needed-var)
           (let ((xxx (assq needed-var bindings-need)))
             (if xxx
                 (cdr xxx)
                 (single-used-var needed-var))))
         body-needs))))
  (define (study-let-values expr live-out)
    (let* ((binding (caadr expr))
           (names (car binding))
           (sub-expr (cadr binding))
           (body (caddr expr)))
      (let ((body-needs (loop body live-out)))
        (define (slot-used? name)
          (var-used? name body-needs))
        (let ((sub-expr-live-out (map slot-used? names)))
          (var-set-union (var-set-difference body-needs names)
                         (loop sub-expr sub-expr-live-out))))))
  (define (study-construction expr live-out)
    (var-set-union*
     (map (lambda (arg)
            (loop arg (list #t)))
          (cdr expr))))
  (define (study-access expr live-out)
    (loop (cadr expr) (list #t)))
  (define (study-values expr live-out)
    (var-set-union*
     (map
      (lambda (live? sub-expr)
        (if live?
            (loop sub-expr (list #t))
            (no-used-vars)))
      live-out
      (cdr expr))))
  (define (study-application expr live-out)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (let* ((operator-i/o-need-map (hash-table/get i/o-need-map operator #f))
             (operand-indecies-needed
              (var-set-union*
               (map
                (lambda (live? index operator-needs)
                  (if live?
                      (begin
                        (output-needed! need-map operator index)
                        operator-needs)
                      (no-used-vars)))
                live-out
                (iota (length live-out))
                operator-i/o-need-map)))
             (operands-needed
              (var-set-map
               (lambda (arg-index)
                 (list-ref operands arg-index))
               operand-indecies-needed)))
        (var-set-union-map
         (lambda (operand)
           (loop operand (list #t)))
         operands-needed))))
  (define improve-need-map
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          ;; This is redundant because the i/o-need-map of this
          ;; operator also allows one to deduce which inputs the
          ;; operator needs, given which of the operator's outputs are
          ;; needed.
          #;
          (inputs-needed! need-map operator
                          (set-map (lambda (var)
                                     (list-index (lambda (arg) (eq? arg var)) args))
                                   (loop body body-live-out)))
          (let ((body-live-out (hash-table/get need-map name #f)))
            (loop body body-live-out))))
  (define (output-needed! need-map name index)
    (let ((needed-outputs (hash-table/get need-map name #f)))
      (if needed-outputs
          (let ((relevant-pair (drop needed-outputs index)))
            (if (car relevant-pair)
                'ok
                (begin
                  (set-car! relevant-pair #t)
                  (changed! need-map))))
          ;; I don't care which inputs of primitives are needed.
          'ok)))
  (improve-need-map defn))

(define (rewrite-definitions i/o-need-map needed-var-map defns)
  (let ((type-map (type-map `(,@defns 'bogon)))) ; This bogon has to do with the entry point being a definition now
    ((on-subexpressions
      (rule `(define ((? name) (?? args))
               (argument-types (?? stuff) (? return))
               (? body))
            (let* ((needed-output-indexes (hash-table/get needed-var-map name #f))
                   (i/o-map (hash-table/get i/o-need-map name #f))
                   (needed-input-indexes (var-set-union*
                                          (map (lambda (live? in-set)
                                                 (if live? in-set (no-used-vars)))
                                               needed-output-indexes i/o-map)))
                   (all-ins-needed? (= (var-set-size needed-input-indexes) (length args)))
                   (all-outs-needed? (every (lambda (x) x) needed-output-indexes)))
              (define new-return-type
                (if (or all-outs-needed? (not (values-form? return)))
                    return
                    `(values ,@(needed-items (cdr return) needed-output-indexes))))
              `(define (,name ,@(needed-items args needed-input-indexes))
                 (argument-types ,@(needed-items stuff needed-input-indexes) ,new-return-type)
                 ,(let ((body (rewrite-call-sites type-map i/o-need-map needed-var-map body)))
                    (let ((the-body (if all-ins-needed?
                                        body
                                        `(let (,(map (lambda (name)
                                                       `(,name ,(make-tombstone)))
                                                     (unneeded-items args needed-input-indexes)))
                                           ,body))))
                      (if all-outs-needed?
                          the-body ; All the outs of the entry point will always be needed
                          (let ((output-names (invent-names-for-parts 'receipt return)))
                            `(let-values ((,output-names ,the-body))
                               (values ,@(needed-items output-names needed-output-indexes)))))))))))
     defns)))

(define (rewrite-call-sites type-map i/o-need-map needed-var-map form)
  (define (procedure? name)
    (hash-table/get needed-var-map name #f))
  ((on-subexpressions
    (rule `((? operator ,procedure?) (?? operands))
          (let* ((needed-output-indexes (hash-table/get needed-var-map operator #f))
                 (i/o-map (hash-table/get i/o-need-map operator #f))
                 (needed-input-indexes (var-set-union*
                                        (map (lambda (live? in-set)
                                               (if live? in-set (no-used-vars)))
                                             needed-output-indexes i/o-map)))
                 (all-outs-needed? (every (lambda (x) x) needed-output-indexes)))
            (let ((the-call
                  ;; TODO One could, actually, eliminate even more
                  ;; dead code than this: imagine a call site that
                  ;; only needs some of the needed outputs of the
                  ;; callee, where the callee only needs some of its
                  ;; needed inputs to compute those outputs.  Then the
                  ;; remaining inputs need to be supplied, because the
                  ;; callee's interface has to support callers that
                  ;; may need the outputs those inputs help it
                  ;; compute, but it would be safe to put tombstones
                  ;; there, because the analysis just proved that they
                  ;; will not be needed.
                  `(,operator ,@(needed-items operands needed-input-indexes))))
              (if all-outs-needed?
                  the-call
                  (let ((output-names
                         (invent-names-for-parts 'receipt (return-type (lookup-type operator)))))
                    `(let-values ((,output-names ,the-call))
                       (values ,@(map (lambda (name index)
                                        (if (var-used? index needed-output-indexes)
                                            name
                                            (make-tombstone)))
                                      output-names
                                      (iota (length output-names)))))))))))
   form))

(define (needed-items items needed-indexes)
  (filter-map
   (lambda (item index)
     (if (var-used? index needed-indexes)
         item
         #f))
   items
   (iota (length items))))

(define (unneeded-items items needed-indexes)
  (filter-map
   (lambda (item index)
     (if (var-used? index needed-indexes)
         #f
         item))
   items
   (iota (length items))))

(define (make-tombstone)
  ;; A tombstone is a value that needs to be supplied but I know will
  ;; never be used.  TODO Make the tombstones distinctive so I can
  ;; check whether they all disappear?
  '())
