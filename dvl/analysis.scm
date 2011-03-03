(declare (usual-integrations))
;;;; Analysis data structure

;;; An analysis binding is a statement about the current state of
;;; knowledge of the analysis, with regard to the evaluation of a
;;; given expression in a given (abstract) environment and in a given
;;; world.  The world contains two pieces of information: the i/o
;;; version (which is an opaque token, with the implication that
;;; distinct such tokens are separated by i/o events but eq? tokens
;;; are not) and the gensym number.  

;;; How can the behavior of expressions depend on the world?  Nothing
;;; in DVL can detect changes in the i/o version over the course of an
;;; analysis --- input from the outside world is already assumed to be
;;; unpredictable.  The only interesting feature of the i/o version is
;;; the knowledge of whether some expression changes it (to wit,
;;; reserves the right to perform i/o).

;;; There is only one DVL primitive whose value can be affected by the
;;; gensym number: GENSYM.  (No DVL means of combination or
;;; abstraction can be directly affected by the gensym number).  The
;;; value of an expression may depend on the gensym number if that
;;; expression generates some gensym and captures it in a data
;;; structure that it returns.  The only DVL primitives that are
;;; affected by the values of gensyms are the gensym comparison
;;; primitives.  Since fresh gensyms by definition compare larger than
;;; all existing gensyms, the incoming gensym number (as opposed to
;;; the modifications to the gensym number that occur in the
;;; evaluation of subexpressions) cannot affect the return value of a
;;; gensym comparison primitive, and therefore cannot affect the
;;; control flow of any expression.

;;; Therefore, whether a returned value does or does not depend on the
;;; incoming gensym number is simply a matter of whether or not it
;;; contains any gensym objects inside.  There is a wrinkle to this:
;;; closures may contain closed-over gensym objects, and yet may have
;;; the property that which gensym they contain cannot be ascertained
;;; operationally.  For example, the closure returned by forward-mode
;;; DERIVATIVE will have this property.  It is not clear offhand how
;;; and whether to detect such cases.

;;; In any case, if the returned value does not depend on the gensym
;;; number, then, by definition, the same value will be returned
;;; regardless of the gensym number obtaining on any subsequent calls.
;;; Therefore, a binding may either key off of the gensym number or
;;; leave that portion as an "any world" placeholder.

;;; The effect an expression has on the world only depends on the
;;; control flow of the expression, and therefore does not depend on
;;; the incoming world.  Furthermore, the only interesting information
;;; about the effect on the world is whether any i/o was done, and the
;;; invariant that the outgoing gensym number must be larger than the
;;; numbers of any gensyms stored in the return value.  This means, in
;;; particular, that if the return value does not depend on the
;;; incoming world, then the expression only has i/o effects (if any)
;;; on the world.

(define-structure (binding safe-accessors)
  exp
  env
  world
  value
  new-world)

(define-structure (analysis safe-accessors)
  bindings)

(define (analysis-search exp env world analysis win lose)
  (let loop ((bindings (analysis-bindings analysis)))
    (if (null? bindings)
	(lose)
	(if (and (equal? exp (binding-exp (car bindings)))
		 (abstract-equal? env (binding-env (car bindings))))
	    (win (car bindings))
	    (loop (cdr bindings))))))

;;; ANALYSIS-GET is \bar E_1 from [1].
(define (analysis-get exp env world analysis win)
  (analysis-search exp env world analysis
   (lambda (binding)
     (win (world-update-value
	   (binding-value binding)
	   world (binding-world binding))
	  (world-update-world
	   (binding-new-world binding)
	   world (binding-world binding))))
   (lambda ()
     (win abstract-none impossible-world))))

(define (simple-analysis-get exp env analysis)
  (analysis-get exp env (initial-dvl-world) analysis
   (lambda (value world) value)))

;;; EXPAND-ANALYSIS is \bar E_1' from [1].
;;; It registers interest in the evaluation of EXP in ENV by producing
;;; a binding to be added to the new incarnation of ANALYSIS, should
;;; the current incarnation lack any binding already covering that
;;; question.
(define (analysis-expand exp env world analysis win)
  (analysis-search exp env world analysis
   (lambda (binding)
     (if (abstract-none? (binding-value binding))
	 '()
	 (win (world-update-value
	       (binding-value binding)
	       world (binding-world binding))
	      (world-update-world
	       (binding-new-world binding)
	       world (binding-world binding)))))
   (lambda ()
     (list (make-binding exp env world abstract-none impossible-world)))))

(define (same-analysis-binding? binding1 binding2)
  (and (equal? (binding-exp binding1) (binding-exp binding2))
       (abstract-equal? (binding-env binding1) (binding-env binding2))
       (world-equal? (binding-world binding1) (binding-world binding2))
       (abstract-equal? (binding-value binding1) (binding-value binding2))
       (world-equal? (binding-new-world binding1) (binding-new-world binding2))))

(define (same-analysis? ana1 ana2)
  (lset= same-analysis-binding? (analysis-bindings ana1)
	 (analysis-bindings ana2)))

(define (step-changed-analysis? ana1 ana2)
  ;; The definition of this is
  ;; (not (same-analysis? ana1 ana2))
  ;; but since ANA2 is made from ANA1 by STEP-ANALYSIS we know that it
  ;; will contain bindings for all the same exp-env pairs (with
  ;; possibly improved values), in the same order, plus maybe a few
  ;; more bindings generated by EXPAND-ANALYSIS.  Therefore, this does
  ;; the right thing (and is faster).
  (let ((bindings1 (analysis-bindings ana1))
	(bindings2 (analysis-bindings ana2)))
    (or (not (= (length bindings1) (length bindings2)))
	(not (every same-analysis-binding? bindings1 bindings2)))))
