(declare (usual-integrations))
;;;; Polyvariant union-free flow analysis by abstract interpretation

;;; This file implements the resursive equations from page 7 of [1].
;;; The strategy is just to write those equations down directly as
;;; code and let it rip.  I'm sure a proper work-list algorithm would
;;; be much faster, but this program is supposed to be expository.

;;; The code extends and completes the presentation in [1] by filling
;;; out all the details.  In particular, [1] omits discussion of
;;; primitives, but when I tried to write down the program for doing
;;; IF it turned out to be somewhat thorny.  IF is the only primitive
;;; in this program that accepts closures and chooses whether to
;;; invoke them; the thorns come from the fact that the analysis has
;;; to be kept from exploring any branch of an IF until that branch is
;;; known to be live.  This program does not include any additional
;;; subtleties that may be introduced by the AD basis.

;;; The main data structure being manipulated here is called an
;;; analysis.  (Sorry about the nomenclature -- "flow analysis" is the
;;; process, but an "analysis" is a particular collection of knowledge
;;; acquired by that process at a particular stage in its progress).
;;; Have a look at analysis.scm if you want representation details.

;;; An analysis is a collection of bindings, binding pairs of
;;; expression and (abstract) environment to abstract values.  Every
;;; such binding means two things.  It means "This is the minimal
;;; shape (abstract value) that covers all the values that I know this
;;; expression, when evalutated in this abstract environment, may
;;; produce during the execution of the program."  It also means "I
;;; want to know more about what this expression may produce if
;;; evaluated in this abstract environment."

;;; The process of abstract interpretation with respect to a
;;; particular analysis consists of two parts: refinement and
;;; expansion.  Refinement tries to answer the question implicit in
;;; every binding by evaluating its expression in its environment for
;;; one step, referring to the analysis for the shapes of
;;; subexpressions and procedure results.  Refinement of one binding
;;; is the definitions of \bar E, \bar A, and \bar E_1 in [1], and
;;; REFINE-EVAL, REFINE-APPLY, and ANALYSIS-GET, respectively, here.

;;; Expansion asks more questions, by creating uninformative (but
;;; inquisitive) bindings for expression-environment pairs that it can
;;; tell would be useful for the refinement of some other, already
;;; present, binding.  Expansion also proceeds by evaluating the
;;; expression of every extant binding in the environment of that
;;; binding for one step, but instead of bottoming out in what the
;;; analysis knows, it only cares about whether the analysis is
;;; curious about those expressions that it finds.  Expansion is \bar
;;; E', \bar A', and \bar E'_1 in [1], and EXPAND-EVAL, EXPAND-APPLY,
;;; and ANALYSIS-EXPAND, respectively, here.

;;; The polyvariance of this flow analysis comes out of the fact that
;;; the same expression is allowed to appear paired with different
;;; abstract environments as the flow analysis proceeds, and the fates
;;; of such copies are henceforth separate.  The union-free-ness of
;;; this flow analysis comes out of the particular set of abstract
;;; values (allowable shapes) used; in particular from the fact that
;;; two shapes are considered compatible only if they differ just in
;;; which real numbers or which booleans occupy parallel slots and
;;; not, notably, if some closures they contain differ as to the
;;; closure's body.  See abstract-values.scm.

;;; [1] Jeffrey Siskind and Barak Pearlmutter, "Using Polyvariant
;;; Union-Free Flow Analysis to Compile a Higher-Order Functional
;;; Programming Language with a First-Class Derivative Operator to
;;; Efficient Fortran-like Code."  Purdue University ECE Technical
;;; Report, 2008.  http://docs.lib.purdue.edu/ecetr/367

;;;; Refinement

;;; REFINE-EVAL is \bar E from [1].
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

;;; REFINE-APPLY is \bar A from [1].
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

(define *on-behalf-of* #f)

;; If you want to look up the value that some exp-env pair evaluates
;; to during flow analysis, it must be because you are trying to
;; refine some other binding.  Therefore, if you would later learn
;; something new about what this exp-env pair evaluates to, then you
;; may need to re-refine said other binding.  Therefore, the lookup
;; should record the bindings on whose behalf exp-env pairs were
;; looked up.  The global variable *on-behalf-of* is the channel for
;; this communication.

;; Also, if you are looking up what some exp-env pair evaluates to
;; during flow analysis, you must have a consistent world in your
;; hand.  If the binding you are looking for already exists, it
;; contains enough information to tell you what that expression and
;; environment will evaluate to in your world (see the discussion in
;; analysis.scm preceding the definition of WORLD-UPDATE-BINDING).
;; If not, the world should be recorded in the new binding that is
;; created.

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
