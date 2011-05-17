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

(define-structure (binding safe-accessors (constructor %make-binding))
  exp
  env
  world
  value
  new-world
  ;; This notify slot is a list of bindings used to keep the analysis
  ;; work queue up to date.
  notify)

(define (make-binding exp env world value new-world)
  (%make-binding exp env world value new-world '()))

(define (register-notification! binding notifee)
  (if (memq notifee (binding-notify binding))
      'ok
      (set-binding-notify! binding (cons notifee (binding-notify binding)))))

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

;;; An analysis is a collection bindings representing all current
;;; knowledge and a queue of those bindings that may be refinable.
;;; The bindings are indexed by the expression-environment pair they
;;; refer to.  The notify slot of an individual binding is the list of
;;; bindings that may be further refined if it happens that this
;;; binding is successfully refined.

(define-structure (analysis safe-accessors (constructor %make-analysis))
  map
  queue)

(define (make-analysis bindings)
  (let* ((binding-map (make-abstract-hash-table))
         (answer (%make-analysis binding-map '())))
    (for-each (lambda (binding)
                (analysis-new-binding! answer binding)) bindings)
    answer))

(define (analysis-bindings analysis)
  (hash-table/datum-list (analysis-map analysis)))

(define (analysis-search exp env analysis win lose)
  (hash-table/lookup (analysis-map analysis) (cons exp env) win lose))

(define (analysis-new-binding! analysis binding)
  (hash-table/put! (analysis-map analysis)
                   (cons (binding-exp binding) (binding-env binding))
                   binding)
  (analysis-notify! analysis binding))

(define (analysis-notify! analysis binding)
  (if (memq binding (analysis-queue analysis))
      'ok
      (set-analysis-queue! analysis (cons binding (analysis-queue analysis)))))

(define (analysis-queue-pop! analysis)
  (if (null? (analysis-queue analysis))
      (error "Popping an empty queue")
      (let ((answer (car (analysis-queue analysis))))
        (set-analysis-queue! analysis (cdr (analysis-queue analysis)))
        answer)))
