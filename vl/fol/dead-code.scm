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
                    (var-set-union pred-used (var-set-union cons-used alt-used)))))))))))
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
              (let ((used (reduce var-set-union (no-used-vars) exprs-used)))
                (win (empty-let-rule
                      `(let ,(map list (map car new-bindings)
                                  new-exprs)
                         ,new-body))
                     (var-set-union used (var-set-difference
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
                         (var-set-union sub-expr-used
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
            (reduce var-set-union (no-used-vars) args-used)))))
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
              (reduce var-set-union (no-used-vars) elts-used))))))
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
           (win new-call (reduce var-set-union (no-used-vars) args-used)))))))
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

(define (var-set-difference vars1 vars2)
  (lset-difference eq? vars1 vars2))

(define var-used? memq)

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
#|
(define (program->procedure-definitions program)
  (define (expression->procedure-definition entry-point)
    `(define (%%main)
       (argument-types real) ;; TODO Get the actual type of the entry point
       ,entry-point))
  (if (begin-form? program)
      (append (cdr (except-last-pair program))
              (list (expression->procedure-definition (last program))))
      (list (expression->procedure-definition program))))

(define (procedure-definitions->program defns)
  (trivial-begin-rule
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
         (rewritten (rewrite-definitions needed-var-map defns)))
    (intraprocedural-dead-variable-elimination ;; TODO Check absence of tombstones
     (procedure-definitions->program
      rewritten))))

(define ((iterate-defn-map initialize improve-locally) defns)
  (let loop ((overall-map (initialize defns))
             (maybe-done? #t))
    (for-each
     (lambda (defn)
       (let ((local-map (improve-locally defn overall-map)))
         (if (equal? local-map (lookup overall-map (definiendum defn)))
             'ok
             (begin
               (add! overall-map (definiendum defn) local-map)
               (set! maybe-done? #f)))))
     defns)
    (if (not maybe-done?)
        (loop overall-map #t)
        overall-map)))

(define compute-i/o-need-map
  (iterate-defn-map initial-i/o-need-map improve-i/o-need-map))

(define (initial-i/o-need-map defns)
  (define (primitive-i/o-need-map)
   (define (nullary name)
     (cons name (vector (no-used-vars))))
   (define (unary name)
     (cons name (vector (single-used-var 0))))
   (define (binary name)
     (cons name (vector (var-set-union (single-used-var 0) (single-used-var 1)))))
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
           (add! answer name
                 (vector-map (lambda (item) (no-used-vars))
                             (all-slots-live return))))
     defns)
    answer))

(define (improve-i/o-need-map defn i/o-need-map)
  (define (loop expr live-out)
    (cond ((fol-var? expr)
           (vector (single-used-var expr)))
          ((number? expr)
           (vector (no-used-vars)))
          ((boolean? expr)
           (vector (no-used-vars)))
          ((null? expr)
           (vector (no-used-vars)))
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
      (let ((pred-needs (loop predicate (vector #t)))
            (cons-needs (loop consequent live-out))
            (alt-needs (loop alternate live-out)))
        (vector-map
         (lambda (live? needed-in)
           (if live?
               (var-set-union (vector-ref pred-needs 0) needed-in)
               (no-used-vars)))
         live-out
         (vector-map var-set-union cons-needs alt-needs)))))
  (define (study-let expr live-out)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (let ((body-needs (loop body live-out))
            (bindings-need (map (lambda (binding)
                                  (cons (car binding)
                                        (vector-ref (loop (cadr binding) (vector #t)) 0)))
                                bindings)))
        (vector-map
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
             (map cons names (vector->list (loop expr (list->vector (map (lambda (x) #t) names)))))))
        (vector-map
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
    (vector
     (reduce var-set-union (no-used-vars)
             (map (lambda (arg)
                    (vector-ref (loop arg (vector #t)) 0))
                  (cdr expr)))))
  (define (study-access expr live-out)
    (loop (cadr expr) (vector #t)))
  (define (study-values expr live-out)
    (vector-map
     (lambda (live? sub-expr)
       (if live?
           (vector-ref (loop sub-expr (vector #t)) 0)
           (no-used-vars)))
     live-out
     (cdr expr)))
  (define (study-application expr live-out)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (let ((operator-i/o-need-map (lookup i/o-need-map operator))
            (operands-need (map (lambda (operand)
                                  (vector-ref (loop operand (vector #t)) 0))
                                operands)))
        (vector-map
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
          (vector-map
           (lambda (out-needs)
             (set-map (lambda (var)
                        (find-index var args))
                      out-needs))
           (loop body (all-slots-live return)))))
  (improve-i/o-need-map defn))

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

(define (initial-need-map defns)
  (let ((answer
         (alist->eq-hash-table
          (map (rule `(define ((? name) (?? args))
                        (argument-types (?? stuff) (? return))
                        (? body))
                     (cons name
                           (cons (vector-map (lambda (x) #f) (all-slots-live return))
                                 (make-vector (length args) #f))))
               defns))))
    (output-needed! answer (definiendum (last defns)) 0)
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
      (let ((pred-needs (loop predicate (vector #t)))
            (cons-needs (loop consequent live-out))
            (alt-needs (loop alternate live-out)))
        (var-set-union pred-needs (var-set-union cons-needs alt-needs)))))
  (define (study-let expr live-out)
    (let ((bindings (cadr expr))
          (body (caddr expr)))
      (let ((body-needs (loop body live-out))
            (bindings-need (map (lambda (binding)
                                  (cons (car binding)
                                        (loop (cadr binding) (vector #t))))
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
        (let ((sub-expr-live-out (list->vector (map slot-used? names))))
          (var-set-union (var-set-difference body-used names)
                 (loop sub-expr sub-expr-live-out))))))
  (define (study-construction expr live-out)
    (reduce var-set-union (no-used-vars)
            (map (lambda (arg)
                   (loop arg (vector #t)))
                 (cdr expr))))
  (define (study-access expr live-out)
    (loop (cadr expr) (vector #t)))
  (define (study-values expr live-out)
    (reduce var-set-union (no-used-vars)
            (map
             (lambda (live? sub-expr)
               (if live?
                   (loop sub-expr (vector #t))
                   (no-used-vars)))
             (vector->list live-out)
             (cdr expr))))
  (define (study-application expr live-out)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (let ((operator-i/o-need-map (lookup i/o-need-map operator))
            (operands-need (map (lambda (operand)
                                  (cons operand (loop operand (vector #t))))
                                operands)))
        (reduce
         var-set-union (no-used-vars)
         (map
          (lambda (live? index)
            (if live?
                (begin
                  (output-needed! need-map operator index)
                  ;; TODO Switch this around to first computing the
                  ;; set of operands that are needed, and then
                  ;; computing which variables that means.
                  (reduce var-set-union (no-used-vars)
                          (map (lambda (arg-index)
                                 (cdr (assq (list-ref operands arg-index) operands-need)))
                               (vector->list
                                (lookup operator-i/o-need-map index)))))
                (no-used-vars)))
          (vector->list live-out)
          (iota (vector-length live-out)))))))
  (define improve-need-map
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          (inputs-needed! need-map operator
                          (set-map (lambda (var)
                                     (find-index var args))
                                   (loop body (lookup need-map name))))))
  (improve-need-map defn))

(define (rewrite-definitions needed-var-map defns)
  ((on-subexpressions
    (rule `(define ((? name) (?? args))
             (argument-types (?? stuff) (? return))
             (? body))
          `(define (,name ,@(needed-args args needed-var-map))
             (argument-types ,@(needed-args stuff needed-var-map)
                             ,(munch-return-type return needed-var-map))
             ,(let ((body (rewrite-call-sites needed-var-map body)))
                (let ((the-body (if (all-ins-needed? name needed-var-map)
                                    body
                                    `(let (,(map (lambda (name)
                                                   `(,name ,(make-tombstone)))
                                                 (unneeded-ins name needed-var-map)))
                                       ,body))))
                  (if (all-outs-needed? name needed-var-map)
                      the-body
                      `(let-values ((,(invent-names-for-parts return) ,the-body))
                         (values ,@(needed-return those-names needed-var-map)))))))))
   defns))

(define (rewrite-call-sites needed-var-map form)
  ((on-subexpressions
    (rule `((? operator ,procedure?) (?? operands))
          (let ((the-call
                 ;; TODO One could, actually, eliminate even more dead
                 ;; code than this: imagine a call site that only
                 ;; needs some of the needed outputs of the callee,
                 ;; where the callee only needs some of its needed
                 ;; inputs to compute those outputs.  Then the
                 ;; remaining inputs need to be supplied, because the
                 ;; callee's interface has to support callers that may
                 ;; need the outputs those inputs help it compute, but
                 ;; it would be safe to put tombstones there, because
                 ;; the analysis just proved that they will not be
                 ;; needed.
                 `(,operator ,@(filter needed? operands))))
            (if (all-outs-needed? operator needed-var-map)
                the-call
                `(let-values ((,(invent-names-for-parts ...) the-call))
                   (values (map (lambda (output)
                                  (if (needed? output operator needed-var-map)
                                      that-name
                                      (make-tombstone)))
                                outputs-of-operator)))))))
   form))
|#
