(declare (usual-integrations))
;;;; Analysis data structure

;;; An analysis binding is a statement about the current state of
;;; knowledge of the analysis, with regard to what the concrete
;;; interpreter would do starting from some point.  The point is
;;; determined by either an expression and an (abstract) environment,
;;; or two abstract values that are a procedure to apply and an
;;; argument to apply it to.  Either kind of point is also in a given
;;; world.  The world represents all accessible information that may
;;; change dynamically during a run of the program (namely, that which
;;; is not encapsulated in the state).  As such, the world contains
;;; one piece of information: the gensym number.  Everything known
;;; about the evaluation is the (abstract) value it returns and the
;;; effect it has on the world (namely, whether or not it creates new
;;; gensyms).

(define-structure (binding safe-accessors (constructor %make-binding (part1 part2 world value new-world notify escapes?)))
  part1
  part2 ; Always an env for exp-env pairs, never an env for proc-arg pairs
  world
  value
  new-world
  ;; This notify slot is a list of bindings used to keep the analysis
  ;; work queue up to date.
  notify
  ;; Is the abstract value represented by this ever visible to the
  ;; outside world?
  escapes?
  (cached-abstract-hash #f))

(define (eval-binding? binding)
  (env? (binding-part2 binding)))

(define (apply-binding? binding)
  (not (eval-binding? binding)))

(define (binding-exp binding)
  (if (eval-binding? binding)
      (binding-part1 binding)
      (internal-error "Trying to take the exp of an apply binding" binding)))

(define (binding-env binding)
  (if (eval-binding? binding)
      (binding-part2 binding)
      (internal-error "Trying to take the env of an apply binding" binding)))

(define (binding-proc binding)
  (if (apply-binding? binding)
      (binding-part1 binding)
      (internal-error "Trying to take the proc of an eval binding" binding)))

(define (binding-arg binding)
  (if (apply-binding? binding)
      (binding-part2 binding)
      (internal-error "Trying to take the arg of an eval binding" binding)))

(define (make-binding key1 key2 world value new-world escapes?)
  (%make-binding key1 key2 world value new-world '() escapes?))

(define (register-notification! binding notifee)
  (if (memq notifee (binding-notify binding))
      'ok
      (set-binding-notify! binding (cons notifee (binding-notify binding)))))

;;; An analysis is a collection of bindings representing all current
;;; knowledge and a queue of those bindings that may be refinable.
;;; The bindings are indexed by the expression-environment pair or the
;;; procedure-arguement pair they refer to.  The notify slot of an
;;; individual binding is the list of bindings that may be further
;;; refined if it happens that this binding is successfully refined.

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

(define (analysis-search key1 key2 analysis win lose)
  (hash-table/lookup (analysis-map analysis) (cons key1 key2) win lose))

(define (analysis-new-binding! analysis binding)
  (hash-table/put! (analysis-map analysis)
                   (cons (binding-part1 binding) (binding-part2 binding))
                   binding)
  (analysis-notify! analysis binding))

(define (analysis-notify! analysis binding)
  (if (memq binding (analysis-queue analysis))
      'ok
      (set-analysis-queue! analysis (cons binding (analysis-queue analysis)))))

(define (analysis-queue-pop! analysis)
  (if (null? (analysis-queue analysis))
      (internal-error "Popping an empty queue")
      (begin1
       (car (analysis-queue analysis))
       (set-analysis-queue! analysis (cdr (analysis-queue analysis))))))

(define (show-analysis analysis)
  (display analysis)
  (newline)
  (map pp (analysis-bindings analysis))
  (pp (analysis-queue analysis)))

;; This one is just for querying the analysis, for example during code
;; generation.  Compare ANALYSIS-GET-IN-WORLD.
(define (analysis-get key1 key2 analysis)
  (analysis-search key1 key2 analysis binding-value (lambda () abstract-none)))
