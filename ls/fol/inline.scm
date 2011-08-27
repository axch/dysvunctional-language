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

(define (inline program)
  (tidy-begin
   (alpha-rename ; Duplicating code bodies may break binding uniqueness
    (%inline program))))

(define (inline-visibly program)
  (tidy-begin
   ((visible-stage alpha-rename)
    ((visible-stage %inline)
     program))))

(define (%inline program)
  (if (begin-form? program)
      (let ((procedure-bodies (inline-map program))
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
      program))

;;; The inline map maps all procedure names to either the lambda
;;; expression that they are equivalent to if it was decided that they
;;; should be inlined, or #f if it was decided that they should not.

(define (inline-map program)
  (let* ((definitions (filter definition? program))
         (defn-map (alist->eq-hash-table
                    (map (lambda (defn)
                           (cons (definiendum defn) defn))
                         definitions)))
         (call-graph
          (map cons definitions
               (map (lambda (defn)
                      ((unique strong-eq-hash-table-type)
                       (filter-map-tree (lambda (leaf)
                                          (hash-table/get defn-map leaf #f))
                                        (definiens defn))))
                    definitions)))
         (non-inlinees (feedback-vertex-set call-graph))
         (inlinees (lset-difference eq? definitions non-inlinees)))
    (map cons (map definiendum inlinees)
         (map definiens (map remove-defn-argument-types inlinees)))))
