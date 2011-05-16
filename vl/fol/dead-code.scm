(declare (usual-integrations))
;;;; Dead code elimination

;;; Variables that hold values that are never used can be eliminated.
;;; The code that computes those values can also be eliminated.

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

;;; In this setup:
;;; - Constants use nothing.
;;; - Variables use themselves.
;;; - If any of the outputs of an IF form are live, then its predicate
;;;   and branches are live, and the IF form uses everything its
;;;   predicate and its branches use.
;;; - A LET body is processed first; any variables (and their
;;;   expressions) the LET binds that its body doesn't use are dead
;;;   and can be skipped.  A LET uses all the variables that are used
;;;   by its body less those bound by the LET itself, and all the
;;;   variables that are used by the expressions that the LET binds to
;;;   its live variables.
;;; - A LET-VALUES is analagous, except that it has only one
;;;   expression binding several variables, some of which may be live
;;;   and others not.  In that case, the LET-VALUES drops the dead
;;;   variables, and recurs on the subexpression giving it that mask
;;;   for what to keep and what to leave off.
;;; - A VALUES form must conversely interpret the incoming mask and
;;;   only keep those of its subexpressions that are actually needed.
;;; - Procedure applications: Since the analysis is intraprocedural,
;;;   it assumes that all the arguments of a live procedure call are
;;;   live.

;;; A procedure call may return multiple values, not all of which may
;;; be wanted by the caller.  The transformation cannot change the set
;;; of values the procedure will return, but the caller will be
;;; tranformed to expect to be given only live values.  At this point,
;;; the transformation pulls a trick: it binds all the values that the
;;; procedure will emit in a LET-VALUES, and immediately returns the
;;; live ones with a VALUES.  The others are bound to a special name
;;; that any future dead variable transform will recognize as "Yes, I
;;; know this variable is dead, but I have to bind it anyway because
;;; it's coming here whether I want it or not."  Analagously,
;;; procedure formal parameters are not removed even if they may be
;;; dead in the body.

;;; In principle, masking similar to what is used for VALUES could be
;;; used to do elimination on the slots of structures, but for
;;; simplicity I have chosen instead to rely on SRA to separate said
;;; structures into individually named variables and just do
;;; elimination on those variables.  I may revisit this decision when
;;; I add union types.

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
          ((fol-const? expr)
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
       (lambda (new-predicate pred-needs)
         (loop consequent live-out
          (lambda (new-consequent cons-needs)
            (loop alternate live-out
             (lambda (new-alternate alt-needs)
               (win `(if ,new-predicate
                         ,new-consequent
                         ,new-alternate)
                    (var-set-union
                     pred-needs (var-set-union cons-needs alt-needs)))))))))))
  (define (eliminate-in-let expr live-out win)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (loop body live-out
       (lambda (new-body body-needs)
         (let ((new-bindings
                (filter (lambda (binding)
                          (var-used? (car binding) body-needs))
                        bindings)))
           (loop* (map cadr new-bindings)
            (lambda (new-exprs exprs-need)
              (let ((used (var-set-union* exprs-need)))
                (win (empty-let-rule
                      `(let ,(map list (map car new-bindings)
                                  new-exprs)
                         ,new-body))
                     (var-set-union
                      used (var-set-difference
                            body-needs (map car bindings))))))))))))
  (define (eliminate-in-let-values expr live-out win)
    (let ((binding (caadr expr))
          (body (caddr expr)))
      (let ((names (car binding))
            (sub-expr (cadr binding)))
        (loop body live-out
         (lambda (new-body body-needs)
           (define (slot-used? name)
             (or (ignore? name)
                 (and (var-used? name body-needs)
                      #t)))
           (let ((sub-expr-live-out (map slot-used? names)))
             (if (any (lambda (x) x) sub-expr-live-out)
                 (loop sub-expr sub-expr-live-out
                  (lambda (new-sub-expr sub-expr-needs)
                    (win (tidy-let-values
                          `(let-values ((,(filter slot-used? names)
                                         ,new-sub-expr))
                             ,new-body))
                         (var-set-union
                          sub-expr-needs
                          (var-set-difference body-needs names)))))
                 (win new-body body-needs))))))))
  ;; Given that I decided not to do proper elimination of dead
  ;; structure slots, I will say that if a structure is needed then
  ;; all of its slots are needed, and any access to any slot of a
  ;; structure causes the entire structure to be needed.
  (define (eliminate-in-construction expr live-out win)
    (loop* (cdr expr)
     (lambda (new-args args-need)
       (win `(,(car expr) ,@new-args)
            (var-set-union* args-need)))))
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
       (lambda (new-elts elts-need)
         (win (if (= 1 (length new-elts))
                  (car new-elts)
                  `(values ,@new-elts))
              (var-set-union* elts-need))))))
  (define (eliminate-in-application expr live-out win)
    (loop* (cdr expr)
     (lambda (new-args args-need)
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
           (win new-call (var-set-union* args-need)))))))
  (define (loop* exprs win)
    (if (null? exprs)
        (win '() '())
        (loop (car exprs) #t
         (lambda (new-expr expr-needs)
           (loop* (cdr exprs)
            (lambda (new-exprs exprs-need)
              (win (cons new-expr new-exprs)
                   (cons expr-needs exprs-need))))))))
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

(define (var-set-equal? vars1 vars2)
  (lset= eq? vars1 vars2))

;;; To do interprocedural dead variable elimination I have to proceed
;;; as follows:
;;; -1) Run a round of intraprocedural dead variable elimination to
;;;     diminish the amount of work in the following (assume all
;;;     procedure calls need all their inputs)
;;; 0) Treat the final expression as a nullary procedure definition
;;; 1) Initialize a map for each procedure, mapping from output that
;;;    might be desired (by index) to set of inputs that are known to
;;;    be needed to compute that output.
;;;    - I know the answer for primitives
;;;    - All compound procedures start mapping every output to the
;;;      empty set of inputs known to be needed.
;;; 2) I can improve the map by walking the body of a procedure,
;;;    carrying down the set of desired outputs and bringing up the
;;;    map saying which outputs require which inputs.  This is exactly
;;;    analagous to the intraprocedural dead code recursion, except
;;;    for distinguishing which inputs are needed for which outputs.
;;;    - Start with all outputs desired.
;;;    - A constant requires no inputs for one output.
;;;    - A variable requires itself for one output.
;;;    - A VALUES maps the requirements of subexpressions to the
;;;      desired outputs.
;;;    - A LET is processed body first:
;;;      - Forward the set of desired outputs to the body, and get the
;;;        map for which of them need what variables from the
;;;        environment.
;;;      - Any bound variables that are not needed for any desired
;;;        output of the body are dead and can be skipped.
;;;      - Process the expressions generating the bound variables that
;;;        are needed for something to determine what they need.
;;;      - For each of its outputs, the LET form as a whole needs the
;;;        variables the body needs for it that the LET didn't bind,
;;;        and all the variables needed by the LET's expressions for
;;;        the variables the body needed that the LET did bind.
;;;    - A LET-VALUES is analagous, except that there is only one
;;;      expression for all the bound names, so it may be given a
;;;      mask.
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
;;;    - I actually only need to store the outputs, because the inputs
;;;      can be deduced from them using the map computed in steps 1-3.
;;;    - All procedures start not needed.
;;;    - The entry point starts fully needed.
;;; 5) I can improve this table by pretending to do an intraprocedural
;;;    dead code elimination on the body of a procedure some of whose
;;;    outputs are needed, except
;;;    - At a procedure call, mark outputs of that procedure as needed
;;;      in the table if I found that I needed them on the walk; then
;;;      take back up the set of things that that procedure says it
;;;      needs to produce what I needed from it.
;;;    - Otherwise walk as for intraprocedural (check this!)
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
       (argument-types ,return-type)
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
      ,(cadddr (last defns)))))

(define (interprocedural-dead-code-elimination program)
  (let* ((defns (program->procedure-definitions program))
         (dependency-map (compute-dependency-map defns))
         (liveness-map ((compute-liveness-map dependency-map) defns))
         (rewritten (rewrite-definitions dependency-map liveness-map defns)))
    (eliminate-intraprocedural-dead-variables ;; TODO Check absence of tombstones
     (procedure-definitions->program
      rewritten))))

;;; The dependency-map is the structure built by steps 1-3 above.  It
;;; maps every procedure name to a list of boolean lists.  The outer
;;; list is parallel to the values that the procedure returns, each
;;; inner list is parallel to the procedure's inputs, and each boolean
;;; answers the question of whether that input is needed for the
;;; procedure to compute that output.

(define (initial-dependency-map defns)
  (define (primitive-dependency-map)
   (define (nullary name)
     (cons name (list '())))
   (define (unary name)
     (cons name (list '(#t))))
   (define (binary name)
     (cons name (list '(#t #t))))
   (alist->eq-hash-table
    `(,@(map nullary '(read-real gensym))
      ;; Type testers real? gensym? null? pair? should never be emitted
      ,@(map unary '(abs exp log sin cos tan asin acos sqrt write-real real
                         zero? positive? negative?))
      ,@(map binary '(+ - * / atan expt < <= > >= = gensym=)))))
  (let ((answer (primitive-dependency-map)))
    (for-each
     (rule `(define ((? name) (?? args))
              (argument-types (?? stuff) (? return))
              (? body))
           (hash-table/put! answer name
            (map (lambda (item) (map (lambda (x) #f) args))
                 (desirable-slot-list return))))
     defns)
    answer))

(define (improve-dependency-map defn dependency-map)
  ;; This loop is like the one in ELIMINATE-IN-EXPRESSION, except that
  ;; 1) It accumulates a more detailed answer structure (namely one
  ;;    mapping all the possible outputs to which variables are needed
  ;;    to compute each one).
  ;; 2) It does not rewrite the expression, so it can be written in
  ;;    direct style.
  ;; 3) It is always interested in all return values, so it needn't
  ;;    pass down a LIVE-OUT list.
  (define (loop expr)
    (cond ((fol-var? expr)
           (list (single-used-var expr)))
          ((fol-const? expr)
           (list (no-used-vars)))
          ((if-form? expr)
           (study-if expr))
          ((let-form? expr)
           (study-let expr))
          ((let-values-form? expr)
           (study-let-values expr))
          ;; If used post SRA, there may be constructions to build the
          ;; answer for the outside world, but there should be no
          ;; accesses.
          ((construction? expr)
           (study-construction expr))
          ((access? expr)
           (study-access expr))
          ((values-form? expr)
           (study-values expr))
          (else ; general application
           (study-application expr))))
  (define (study-if expr)
    (let ((predicate (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
      (let ((pred-needs (car (loop predicate)))
            (cons-needs (loop consequent))
            (alt-needs (loop alternate)))
        (map
         (lambda (needed-in)
           (var-set-union pred-needs needed-in))
         (map var-set-union cons-needs alt-needs)))))
  (define (study-let expr)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (let ((body-needs (loop body))
            (bindings-need (map (lambda (binding)
                                  (cons (car binding)
                                        (car (loop (cadr binding)))))
                                bindings)))
        (map (interpolate-let-bindings bindings-need) body-needs))))
  (define (study-let-values expr)
    (let* ((binding (caadr expr))
           (names (car binding))
           (sub-expr (cadr binding))
           (body (caddr expr)))
      (let ((body-needs (loop body))
            (bindings-need (map cons names (loop sub-expr))))
        (map (interpolate-let-bindings bindings-need) body-needs))))
  (define (interpolate-let-bindings bindings-need)
    (lambda (need-set)
      (var-set-union-map
       (lambda (needed-var)
         (let ((binding.need (assq needed-var bindings-need)))
           (if binding.need
               (cdr binding.need)
               (single-used-var needed-var))))
       need-set)))
  (define (study-construction expr)
    (list
     (var-set-union*
      (map (lambda (arg)
             (car (loop arg)))
           (cdr expr)))))
  (define (study-access expr)
    (loop (cadr expr)))
  (define (study-values expr)
    (map
     (lambda (sub-expr)
       (car (loop sub-expr)))
     (cdr expr)))
  (define (study-application expr)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (let ((operator-dependency-map (hash-table/get dependency-map operator #f))
            (operands-need (map (lambda (operand)
                                  (car (loop operand)))
                                operands)))
        (map
         (lambda (operands-live)
           (var-set-union*
            (select-masked
             operands-live
             operands-need)))
         operator-dependency-map))))
  (define improve-dependency-map
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          (map
           (lambda (out-needs)
             (map (lambda (arg)
                    (and (var-used? arg out-needs) #t))
                  args))
           (loop body))))
  (improve-dependency-map defn))

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
         (if (every equal? local-map (hash-table/get overall-map (definiendum defn) #f))
             'ok
             (begin
               (hash-table/put! overall-map (definiendum defn) local-map)
               (set! maybe-done? #f)))))
     defns)
    (if (not maybe-done?)
        (loop overall-map #t)
        overall-map)))

(define compute-dependency-map
  (iterate-defn-map initial-dependency-map improve-dependency-map))

;;; The liveness map is the structure constructed during steps 4-6
;;; above.  It maps every procedure name to the set of its outputs
;;; that are actually needed (represented as a parallel list of
;;; booleans).  The needed inputs can be inferred from this given the
;;; dependency-map.

(define ((compute-liveness-map dependency-map) defns)
  (let ((liveness-map (initial-liveness-map defns)))
    (let loop ()
      (clear-changed! liveness-map)
      (for-each
       (lambda (defn)
         ((improve-liveness-map! dependency-map) defn liveness-map))
       defns)
      (if (changed? liveness-map)
          (loop)
          liveness-map))))

(define (clear-changed! thing)
  (eq-put! thing 'changed #f))

(define (changed? thing)
  (eq-get thing 'changed))

(define (changed! thing)
  (eq-put! thing 'changed #t))

(define (initial-liveness-map defns)
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

(define ((improve-liveness-map! dependency-map) defn liveness-map)
  ;; This loop is identical with the one in ELIMINATE-IN-EXPRESSION,
  ;; except that
  ;; 1) It doesn't rewrite the expression (so can be in direct style)
  ;; 2) It refers to the given DEPENDENCY-MAP for what callees need
  ;; 3) When it encounters a procedure call, it updates the
  ;;    LIVENESS-MAP with a side effect.
  (define (loop expr live-out)
    (cond ((fol-var? expr)
           (single-used-var expr))
          ((fol-const? expr)
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
      (let ((body-needs (loop body live-out)))
        (var-set-union-map
         (lambda (needed-var)
           (let ((binding (assq needed-var bindings)))
             (if binding
                 (loop (cadr binding) (list #t))
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
     (map (lambda (sub-expr) (loop sub-expr (list #t)))
          (select-masked live-out (cdr expr)))))
  (define (study-application expr live-out)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (outputs-needed! liveness-map operator live-out)
      (let* ((operator-dependency-map (hash-table/get dependency-map operator #f))
             (operands-live
              (find-needed-inputs live-out operator-dependency-map)))
        (var-set-union*
         (map (lambda (operand) (loop operand (list #t)))
              (select-masked operands-live operands))))))
  (define improve-liveness-map
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          (let ((body-live-out (hash-table/get liveness-map name #f)))
            ;; The loop does return information on which inputs are
            ;; needed, but I can safely throw it away because the
            ;; dependency-map of this operator already allows one to
            ;; deduce which inputs the operator needs, given which of
            ;; the operator's outputs are needed.
            (loop body body-live-out))))
  (define (outputs-needed! liveness-map name live)
    (let ((needed-outputs (hash-table/get liveness-map name #f)))
      (if needed-outputs
          (pair-for-each
           (lambda (known-needed new-needed)
             (if (and (car new-needed) (not (car known-needed)))
                 (begin
                   (set-car! known-needed #t)
                   (changed! liveness-map))
                 'ok))
           needed-outputs
           live)
          ;; I don't care which outputs of primitives are needed.
          'ok)))
  (improve-liveness-map defn))

(define (rewrite-definitions dependency-map liveness-map defns)
  (let ((type-map (type-map `(begin ,@defns 'bogon)))) ; This bogon has to do with the entry point being a definition now
    ((on-subexpressions
      (rule `(define ((? name) (?? args))
               (argument-types (?? stuff) (? return))
               (? body))
            (let* ((needed-outputs (hash-table/get liveness-map name #f))
                   (i/o-map (hash-table/get dependency-map name #f))
                   (needed-inputs (find-needed-inputs needed-outputs i/o-map))
                   (all-ins-needed? (every (lambda (x) x) needed-inputs))
                   (all-outs-needed? (every (lambda (x) x) needed-outputs)))
              (define new-return-type
                (if (or all-outs-needed? (not (values-form? return)))
                    return
                    (tidy-values
                     `(values ,@(select-masked
                                 needed-outputs (cdr return))))))
              `(define (,name ,@(select-masked needed-inputs args))
                 (argument-types ,@(select-masked needed-inputs stuff)
                                 ,new-return-type)
                 ,(let ((body (rewrite-call-sites
                               type-map dependency-map liveness-map body)))
                    (let ((the-body
                           (if all-ins-needed?
                               body
                               `(let (,(map (lambda (name)
                                              `(,name ,(make-tombstone)))
                                            (select-masked
                                             (map not needed-inputs) args)))
                                  ,body))))
                      (if all-outs-needed?
                          the-body ; All the outs of the entry point will always be needed
                          (let ((output-names
                                 (invent-names-for-parts 'receipt return)))
                            (tidy-let-values
                             `(let-values ((,output-names ,the-body))
                                ,(tidy-values
                                  `(values
                                    ,@(select-masked
                                       needed-outputs output-names)))))))))))))
     defns)))

(define (rewrite-call-sites type-map dependency-map liveness-map form)
  (define (procedure? name)
    (hash-table/get liveness-map name #f))
  ((on-subexpressions
    (rule `((? operator ,procedure?) (?? operands))
          (let* ((needed-outputs (hash-table/get liveness-map operator #f))
                 (i/o-map (hash-table/get dependency-map operator #f))
                 (needed-inputs (find-needed-inputs needed-outputs i/o-map))
                 (all-outs-needed? (every (lambda (x) x) needed-outputs)))
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
                  `(,operator ,@(select-masked needed-inputs operands))))
              (if all-outs-needed?
                  the-call
                  (let ((output-names
                         (invent-names-for-parts
                          'receipt (return-type (type-map operator)))))
                    (let ((needed-names
                           (select-masked needed-outputs output-names)))
                      (tidy-let-values
                       `(let-values ((,needed-names ,the-call))
                          ,(tidy-values
                            `(values ,@(map (lambda (live? name)
                                              (if live?
                                                  name
                                                  (make-tombstone)))
                                            needed-outputs
                                            output-names))))))))))))
   form))

(define (find-needed-inputs needed-outputs i/o-map)
  (define (parallel-list-and lists)
    (reduce (lambda (input answer)
              (map boolean/and input answer))
            (map (lambda (x) #f) (car i/o-map))
            lists))
  (parallel-list-and
   (select-masked needed-outputs i/o-map)))

(define (select-masked liveness items)
  (filter-map (lambda (live? item)
                (and live? item))
              liveness items))

(define (make-tombstone)
  ;; A tombstone is a value that needs to be supplied but I know will
  ;; never be used.  TODO Make the tombstones distinctive so I can
  ;; check whether they all disappear?
  '())
