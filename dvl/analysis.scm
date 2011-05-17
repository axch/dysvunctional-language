(declare (usual-integrations))
;;;; Analysis data structure

;;; An analysis binding is a statement about the current state of
;;; knowledge of the analysis, with regard to the evaluation of a
;;; given expression in a given (abstract) environment and in a given
;;; world.  The world represents all accessible information that may
;;; change dynamically during a run of the program (namely, that which
;;; is not encapsulated in the expression to evaluate and its abstract
;;; environment).  As such, the world contains one piece of
;;; information: the gensym number.  Everything known about the
;;; evaluation of an expression is the (abstract) value it returns and
;;; the effect it has on the world (namely, whether or not it creates
;;; new gensyms).

(define-structure (binding safe-accessors)
  exp
  env
  world
  value
  new-world
  ;; This notify slot helps update the analysis with a work list
  ;; instead of round-robin.
  notify)

;;; How can the behavior of expressions depend on the world?  There is
;;; only one DVL primitive whose value can be affected by the gensym
;;; number: GENSYM.  (No DVL means of combination or abstraction can
;;; be directly affected by the gensym number).  The value of an
;;; expression may depend on the gensym number if that expression
;;; generates some gensym and captures it in a data structure that it
;;; returns.  The only DVL primitive that is affected by the values of
;;; gensyms is GENSYM=.  Since fresh gensyms by definition compare
;;; larger than all existing gensyms, the incoming gensym number (as
;;; opposed to the modifications to the gensym number that occur in
;;; the evaluation of subexpressions) cannot affect the return value
;;; of a gensym comparison primitive, and therefore cannot affect the
;;; control flow of any expression.

;;; Consequently, assuming the consistency condition that the incoming
;;; gensym number is always larger than all gensyms stored in the
;;; environment, the return value will always have the same shape,
;;; contain the same gensyms that are less than the gensym number, and
;;; contain gensyms with the same positive offsets from the gensym
;;; number, regardless of what the incoming gensym number actually is.
;;; Likewise, the new world produced on evaluation of the expression
;;; will be offset from the incoming gensym number by a fixed amount.

(define (world-update-binding binding new-world win)
  (win (world-update-value
        (binding-value binding)
        (binding-world binding)
        new-world)
       (world-update-world
        (binding-new-world binding)
        (binding-world binding)
        new-world)))

(define (same-analysis-binding? binding1 binding2)
  (and (equal? (binding-exp binding1) (binding-exp binding2))
       (abstract-equal? (binding-env binding1) (binding-env binding2))
       (world-equal? (binding-world binding1) (binding-world binding2))
       (abstract-equal? (binding-value binding1) (binding-value binding2))
       (world-equal? (binding-new-world binding1) (binding-new-world binding2))))
;;; EXPAND-ANALYSIS is \bar E_1' from [1].
;;; It registers interest in the evaluation of EXP in ENV by producing
;;; a binding to be added to the new incarnation of ANALYSIS, should
;;; the current incarnation lack any binding already covering that
;;; question.
(define (analysis-expand exp env world analysis win)
  (analysis-search exp env analysis
   (lambda (binding)
     (if (abstract-none? (binding-value binding))
         '()
         (world-update-binding binding world win)))
   (lambda ()
     (list (make-binding exp env world abstract-none impossible-world)))))

