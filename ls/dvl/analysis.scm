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
  state ; Either an eval-state or an apply-state
  world
  value
  new-world
  ;; This notify slot is a list of bindings used to keep the analysis
  ;; work queue up to date.
  notify)

(define-structure (eval-state safe-accessors)
  exp
  env)

(define (binding-exp binding)
  (eval-state-exp (binding-state binding)))

(define (binding-env binding)
  (eval-state-env (binding-state binding)))

(define (make-binding exp env world value new-world)
  (%make-binding (make-eval-state exp env) world value new-world '()))

(define (register-notification! binding notifee)
  (if (memq notifee (binding-notify binding))
      'ok
      (set-binding-notify! binding (cons notifee (binding-notify binding)))))

;;; An analysis is a collection of bindings representing all current
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

(define (show-analysis analysis)
  (display analysis)
  (newline)
  (map pp (analysis-bindings analysis))
  (pp (analysis-queue analysis)))

;; This one is just for querying the analysis, for example during code
;; generation.  Compare ANALYSIS-GET-IN-WORLD.
(define (analysis-get exp env analysis)
  (analysis-search exp env analysis binding-value (lambda () abstract-none)))
