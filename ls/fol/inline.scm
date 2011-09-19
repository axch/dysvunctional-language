(declare (usual-integrations))
;;;; Inlining procedure definitions

;;; Procedure definitions can be inlined arbitrarily, provided that
;;; you don't try to inline all the edges of any cycle of the call
;;; graph.  Since FOL is first order, the static call graph is a
;;; decent approximation of the dynamic call graph, and the actual
;;; inlining can be done by search and replace of call sites.  There
;;; is one interesting step: to decide which procedures not to inline,
;;; I need to compute a feedback vertex set (set of vertices whose
;;; removal makes acyclic) of the static call graph.  See
;;; feedback-vertex-set.scm for discussion and implementation.

(define (%inline program)
  (%%inline (count-pairs program) program))

(define (%%inline size-increase-threshold program)
  (tidy-begin
   (if (begin-form? program)
       (let ((procedure-bodies (inline-map size-increase-threshold program))
             (walked-bodies (make-eq-hash-table)))
         (define (inline? name)
           (not (not (walked-body name))))
         (define (not-inline? form)
           (or (not (definition? form))
               (not (inline? (definiendum form)))))
         (define (walk expression)
           ((on-subexpressions
             (rule `((? name ,inline?) (?? args))
                   (->let `(,(force (walked-body name)) ,@args))))
            expression))
         (define (walked-body name)
           (hash-table/get walked-bodies name #f))
         (for-each (lambda (datum)
                     (hash-table/put! walked-bodies (car datum)
                                      (delay (walk (cdr datum)))))
                   procedure-bodies)
         (walk (filter not-inline? program)))
       program)))

;;; The inline map maps all procedure names to either the lambda
;;; expression that they are equivalent to if it was decided that they
;;; should be inlined, or #f if it was decided that they should not.

(define (inline-map size-increase-threshold program)
  (let* ((lookup (definition-map program))
         (call-graph (call-graph program))
         (inlinee-names (acceptable-inlinees size-increase-threshold call-graph)))
    (map cons inlinee-names
         (map definiens
              (map remove-defn-argument-types
                   (map lookup inlinee-names))))))

(define (definition-map program)
  (define defn-map
    (alist->eq-hash-table
     (map (lambda (defn)
            (cons (definiendum defn) defn))
          (filter definition? program))))
  (lambda (name)
    (hash-table/get defn-map name #f)))

(define (call-graph program)
  (define (defn-inline-cost defn)
    (+ (count-pairs
        (definiens (remove-defn-argument-types defn)))
       ;; Let bindings, being a list of two-element lists, take a
       ;; little more space than an apply with an explicit lambda,
       ;; because that is two parallel lists.
       (length (cdr (cadr defn)))))
  (define (make-record id cost out-neighbors)
    (cons id (cons cost out-neighbors)))
  (let ((defined-name? (definition-map program)))
    (cons
     (let ((entry-point-name (make-name '%%main)))
       (make-record
        entry-point-name 0
        (cons entry-point-name ; Entry point is not inlinable
              (filter-tree defined-name? (entry-point program)))))
     (map (lambda (defn)
            (make-record
             (definiendum defn)
             (defn-inline-cost defn)
             (filter-tree defined-name? (definiens defn))))
          (filter definition? program)))))
