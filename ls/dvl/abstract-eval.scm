(declare (usual-integrations))
;;;; Polyvariant union-free flow analysis by abstract interpretation

;;; The following discussion assumes that the reader is familiar with
;;; the implementation of VL.  Like in VL, the main data structure
;;; being manipulated here is called an analysis.  We adopt the
;;; nomenclature (arguably, somewhat unfortunate) in which "flow
;;; analysis" is the process, while "analysis" is a particular
;;; collection of knowledge acquired by that process at a particular
;;; stage in its progress.  See analysis.scm for representation
;;; details.

;;; An analysis is a collection of bindings.  Every binding means two
;;; things.  It means "This is the minimal shape (abstract value) that
;;; covers all the values that I know this expression, when evaluated
;;; in this abstract environment and in this world, may produce during
;;; the execution of the program."  It also means "I want to know more
;;; about what this expression may produce if evaluated in this
;;; abstract environment and in this world."  Although the value of an
;;; expression in an environment depends on the world in which the
;;; evaluation takes place, this dependence is actually very simple,
;;; and in particular, knowing the shape of an expression evaluated in
;;; some abstract environment and in some world is sufficient to be
;;; able to tell what that expression will evaluate to in the same
;;; abstract environment and in any larger world; see the discussion
;;; below.

;;; The following implementation of abstract interpretation differs
;;; significantly from that used in VL.  We recall that in VL the
;;; process of abstract interpretation with respect to a particular
;;; analysis consists of two parts: refinement and expansion.
;;; Refinement tries to answer the question implicit in every binding
;;; by evaluating its expression in its environment in one step,
;;; referring to the analysis for the shapes of subexpressions and
;;; procedure results, and bottoming out if some of these shapes are
;;; unknown.  Expansion takes these unknown shapes into account by
;;; creating new bottom-valued bindings for the expression-environment
;;; pairs whose values are not known yet to the flow analysis but which
;;; occur during refinement.  The process repeats until the analysis
;;; contains no bottom-valued bindings.

;;; This approach, despite its conceptual simplicity and clarity, is
;;; very inefficient: a binding can be refined many times even if it
;;; doesn't need refinement anymore.  In DVL, this inefficiency
;;; becomes very noticeable, and as a consequence a more efficient
;;; work-list algorithm is used.  Instead of refining at each step
;;; every binding of the analysis, we maintain a queue of the bindings
;;; that may need refinement.  Furthermore, every binding maintains a
;;; list of bindings that depend on it: if refinement of binding A
;;; finds that it needs binding B, it adds A to B's list of dependent
;;; bindings.  Conversely, if some binding is refined and its value
;;; has changed, the dependent bindings are put into the queue for
;;; possible refinement.  Expansion then becomes part of the
;;; refinement process: when an environment-expression pair is not
;;; found in the analysis, a new bottom-valued binding is immediately
;;; created for it and is placed into the work queue.

;;;; Refinement

;;; REFINE-EVAL is the counterpart of E from [1].
(define (refine-eval exp env world analysis win)
  (cond ((constant? exp) (win (constant-value exp) world))
        ((variable? exp) (win (lookup exp env) world))
        ((null? exp) (win '() world))
        ((lambda-form? exp)
         (win (make-closure exp env) world))
        ((pair-form? exp)
         (get-during-flow-analysis (car-subform exp) env world analysis
          (lambda (car-value car-world)
            (get-during-flow-analysis (cdr-subform exp) env car-world analysis
             (lambda (cdr-value cdr-world)
               (if (and (not (abstract-none? car-value))
                        (not (abstract-none? cdr-value)))
                   (win (cons car-value cdr-value) cdr-world)
                   (win abstract-none impossible-world)))))))
        ((application? exp)
         (get-during-flow-analysis (operator-subform exp) env world analysis
          (lambda (operator operator-world)
            (get-during-flow-analysis (operand-subform exp) env operator-world analysis
             (lambda (operand operand-world)
               (refine-apply operator operand operand-world analysis win))))))
        (else
         (error "Invalid expression in abstract refiner"
                exp env analysis))))

;;; REFINE-APPLY is the counterpart of A from [1].
(define (refine-apply proc arg world analysis win)
  (cond ((abstract-none? arg) (win abstract-none impossible-world))
        ((abstract-none? proc) (win abstract-none impossible-world))
        ((primitive? proc)
         ((primitive-abstract-implementation proc) arg world analysis win))
        ((closure? proc)
         (get-during-flow-analysis
          (closure-body proc)
          (extend-env (closure-formal proc) arg (closure-env proc))
          world
          analysis
          win))
        (else
         (error "Refining an application of a known non-procedure"
                proc arg analysis))))

;;;; Flow analysis

;;; The flow analysis proper starts with a one-binding analysis
;;; representing the whole program and incrementally improves
;;; it until done.

(define (analyze program)
  (let ((analysis (initial-analysis (macroexpand program))))
    (let loop ((count 0))
      (do-wallpaper count analysis *analyze-wallp*)
      (if (null? (analysis-queue analysis))
          (begin
            ;; 0 always triggers any wallpaper.
            (do-wallpaper 0 analysis *analyze-wallp*)
            analysis)
          (begin
            (refine-next-binding! analysis)
            (loop (+ count 1)))))))

;; If this is a number n, show the analysis every n steps.
;; If this is a procedure, call it (for effect) on each new analysis.
;; If this is a pair of a number n and a procedure, call that procedure
;;   every n steps (and once more for the last analysis).
(define *analyze-wallp* #f)

(define (do-wallpaper count analysis wallp-spec)
  (cond ((and (pair? wallp-spec)
              (= 0 (modulo count (car wallp-spec))))
         ((cdr wallp-spec) analysis))
        ((number? wallp-spec)
         (do-wallpaper count analysis (cons wallp-spec show-analysis)))
        ((procedure? wallp-spec)
         (do-wallpaper count analysis (cons 1 wallp-spec)))
        (else 'ok)))

(define (initial-analysis program)
  (make-analysis
   (list
    (make-binding
     program
     (initial-user-env)
     (initial-world)
     abstract-none
     impossible-world))))

(define (refine-next-binding! analysis)
  (let ((binding (analysis-queue-pop! analysis)))
    (fluid-let ((*on-behalf-of* binding))
      (let ((exp (binding-exp binding))
            (env (binding-env binding))
            (world (binding-world binding))
            (value (binding-value binding)))
        (refine-eval
         exp env world analysis
         (lambda (new-value new-world)
           ;; The following requires an explanation.  Why are we
           ;; taking the union of the old value of the binding with
           ;; the new, refined value?  The thing is, REFINE-EVAL is
           ;; not monotonic; i.e., it can return ABSTRACT-NONE even
           ;; for a binding that is already known to evaluate to a
           ;; non-bottom; see an example below.  Although this also
           ;; happens in the gensym-free VL, accidentally this
           ;; does not bite us because every time the analysis is
           ;; refined each of its bindings is refined; whereas here
           ;; new bindings are pushed onto the front of the work
           ;; queue, and may potentially never be refined because the
           ;; analyzer is too busy refining other bindings on the
           ;; front of the queue.  This can probably be solved by
           ;; changing the order in which bindings are refined (e.g.,
           ;; by pushing new bindings on the rear of the queue and
           ;; popping bindings to work on from the front of the
           ;; queue).  However, the proper way to fix it is to ensure
           ;; that no information is lost, which can be achieved
           ;; e.g. by taking the union of the old value with the new
           ;; value.

           ;; TODO Make VL not depend on the way analysis is refined
           ;; by adding ABSTRACT-UNION in an appropriate place.
           (let ((new-value (abstract-union value new-value)))
             ;; The following check is necessary, because if we
             ;; blindly execute the BEGIN block, it will restore the
             ;; binding in the queue, thus creating an infinite loop.
             (if (abstract-equal? value new-value)
                 'ok
                 (begin
                   ;; Alert for the fans of pure FP: both ANALYSIS and
                   ;; BINDING are mutable objects and the following
                   ;; will modify these objects in place.
                   (set-binding-value! binding new-value)
                   (set-binding-new-world! binding new-world)
                   (for-each (lambda (dependency)
                               (analysis-notify! analysis dependency))
                             (binding-notify binding)))))))))))

;; Example that shows that REFINE-EVAL is not monotonic:
#|
 (let ((my-* (lambda (x y) (* x y))))
   (letrec ((fact (lambda (n)
                    (if (= n 1)
                        1
                        (my-* n (fact (- n 1)))))))
     (fact (real 5))))
|#
;; TODO Rather than sprinkling ABSTRACT-UNION in the places where
;; REFINE-EVAL is used, the definition of REFINE-EVAL needs to be
;; changed in order to make REFINE-EVAL monotonic.

;; If you want to look up the value that some exp-env pair evaluates
;; to during flow analysis, it must be because you are trying to
;; refine some other binding.  Therefore, if you would later learn
;; something new about what this exp-env pair evaluates to, then you
;; may need to re-refine said other binding.  Therefore, the lookup
;; should record the bindings on whose behalf exp-env pairs were
;; looked up.  The global variable *on-behalf-of* is the channel for
;; this communication.

(define *on-behalf-of* #f)

;; How can the behavior of expressions depend on the world?  There is
;; only one DVL primitive whose value can be affected by the gensym
;; number: GENSYM.  (No DVL means of combination or abstraction can be
;; directly affected by the gensym number).  The value of an
;; expression may depend on the gensym number if that expression
;; generates some gensym and captures it in a data structure that it
;; returns.  The only DVL primitive that is affected by the values of
;; gensyms is GENSYM=.  Since fresh gensyms by definition compare
;; larger than all existing gensyms, the incoming gensym number (as
;; opposed to the modifications to the gensym number that occur in the
;; evaluation of subexpressions) cannot affect the return value of a
;; gensym comparison primitive, and therefore cannot affect the
;; control flow of any expression.

;; Consequently, assuming the consistency condition that the incoming
;; gensym number is always larger than all gensyms stored in the
;; environment, the return value will always have the same shape,
;; contain the same gensyms that are less than the gensym number, and
;; contain gensyms with the same positive offsets from the gensym
;; number, regardless of what the incoming gensym number actually is.
;; Likewise, the new world produced on evaluation of the expression
;; will be offset from the incoming gensym number by a fixed amount.

;; If you are looking up what some exp-env pair evaluates to during
;; flow analysis, then you must have a consistent world in your hand.
;; If the binding you are looking for already exists, it contains, by
;; the above discussion, enough information to tell you what that
;; expression and environment will evaluate to in your world.  Indeed,
;; you only need to update the gensyms contained in the value of the
;; binding appropriately.  This functionality is abstracted in the
;; form of the procedure WORLD-UPDATE-BINDING below.  If no binding
;; with the given exp-env pair exists, the world should be recorded in
;; the new binding that is created.

;; Contrast this complexity with ANALYSIS-GET.
(define (get-during-flow-analysis exp env world analysis win)
  (if (not *on-behalf-of*)
      (error "get-during-flow-analysis must always be done on behalf of some binding"))
  (define (search-win binding)
    (register-notification! binding *on-behalf-of*)
    (world-update-binding binding world win))
  (analysis-search exp env analysis
   search-win
   (lambda ()
     (if (impossible-world? world)
         (win abstract-none impossible-world)
         (let ((binding (make-binding exp env world abstract-none impossible-world)))
           (analysis-new-binding! analysis binding)
           (search-win binding))))))

;; TODO Can we prove that WORLD-UPDATE-BINDING is always going to be
;; called with BINDINGs the worlds of which are not greater than
;; NEW-WORLD?
(define (world-update-binding binding new-world win)
  (win (world-update-value
        (binding-value binding)
        (binding-world binding)
        new-world)
       (world-update-world
        (binding-new-world binding)
        (binding-world binding)
        new-world)))

;; Shift every gensym number contained in THING, except those smaller
;; than OLD-WORLD, by the difference between NEW-WORLD and OLD-WORLD.
(define (world-update-value thing old-world new-world)
  (if (or (impossible-world? new-world)
          (impossible-world? old-world)
          (world-equal? old-world new-world))
      thing
      (let loop ((thing thing))
        (cond ((abstract-gensym? thing)
               (make-abstract-gensym
                (world-update-gensym-number
                 (abstract-gensym-min thing) old-world new-world)
                (world-update-gensym-number
                 (abstract-gensym-max thing) old-world new-world)))
              (else (object-map loop thing))))))

(define (world-update-world updatee old-world new-world)
  (if (or (impossible-world? new-world)
          (impossible-world? old-world)
          (impossible-world? updatee))
      updatee
      (make-world
       (+ (world-gensym updatee)
          (- (world-gensym new-world)
             (world-gensym old-world))))))

(define (world-update-gensym-number number old-world new-world)
  (if (< number (world-gensym old-world))
      ;; Already existed
      number
      ;; Newly made
      (+ number (- (world-gensym new-world) (world-gensym old-world)))))
