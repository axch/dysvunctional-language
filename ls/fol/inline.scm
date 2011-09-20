(declare (usual-integrations))
;;;; Inlining procedure definitions

;;; Inlining procedure definitions produces the following benefits:
;;; - Procedure boundaries are removed, giving later intraprocedural
;;;   optimizations more purchase.
;;; - Procedure bodies are cloned to their call sites, allowing
;;;   per-call-site specializations and optimizations.
;;; - Function call costs in the underlying implementation are
;;;   removed.

;;; Inlining procedure definitions also has the drawback of
;;; (potentially) increasing code size, causing:
;;; - Later stages to repeat work
;;; - Worse instruction cache performance in the final output

;;; This inliner makes the simplification that it will inline all or
;;; none of the call sites of any given procedure.  Given that, the
;;; question of selecting which procedures to inline can be reduced to
;;; a choice of vertices of the static call graph.  Since FOL is first
;;; order, the task of actually inlining the selected procedures
;;; becomes a search-and-replace of their names by their bodies.

(define (perform-inlining program names)
  (let ((lookup-defn (definition-map program))
        (walked-bodies (make-eq-hash-table)))
    ;; Memoize performing the inlining on a particular procedure's
    ;; body in an explicit hash table of promises to avoid repeating
    ;; it for every call site.
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
    (for-each (lambda (name)
                (hash-table/put!
                 walked-bodies name
                 (delay (walk (definiens (remove-defn-argument-types
                                          (lookup-defn name)))))))
              names)
    (walk (filter not-inline? program))))

;;; So, which vertices of the call graph should be inlined?  The
;;; current strategy is to annotate the call graph with the
;;; multiplicity of its edges and with the per-call-site code size
;;; increase from inlining each procedure, and then greedily choose to
;;; inline the procedures that give the smallest overall increase
;;; until the size hits a threshold.

;;; This annotated call graph is represented as a list of records of
;;; the form
;;;   (procedure-name inline-cost . callee-names)
;;; where the list of callee names admits duplicates to indicate
;;; multiplicity.  The procedure CALL-GRAPH computes this graph for a
;;; given program.

(define (call-graph program)
  (define defined-name? (definition-map program))
  (define (make-record id cost out-neighbors)
    (cons id (cons cost out-neighbors)))
  (define (defn-inline-cost defn)
    (+ (count-pairs
        (definiens (remove-defn-argument-types defn)))
       ;; Let bindings, being a list of two-element lists, take a
       ;; little more space than an apply with an explicit lambda,
       ;; because that is two parallel lists.
       (length (cdr (cadr defn)))))
  (define (defn-vertex defn)
    (make-record
     (definiendum defn)
     (defn-inline-cost defn)
     (filter-tree defined-name? (definiens defn))))
  (define entry-point-vertex
    (let ((entry-point-name (make-name '%%main)))
      (make-record
       entry-point-name 0
       (cons entry-point-name ; Entry point is not inlinable
             (filter-tree defined-name? (entry-point program))))))
  (cons
   entry-point-vertex
   (map defn-vertex (filter definition? program))))

;;; The actual greedy graph algorithm is implemented by
;;; ACCEPTABLE-INLINEES in inlinees.scm.  So the toplevel inliner just
;;; picks a threshold and performs the inlining indicated by
;;; ACCEPTABLE-INLINEES on the call graph of the program.

(define (%inline program)
  (%%inline (count-pairs program) program))

(define (%%inline size-increase-threshold program)
  (tidy-begin
   (if (begin-form? program)
       (perform-inlining
        program
        (acceptable-inlinees
         size-increase-threshold (call-graph program)))
       program)))

;;; Historical note: in the previous world order, the set of vertices
;;; not to inline was chosen as a feedback vertex set (set of vertices
;;; whose removal makes acyclic) of the static call graph, and the
;;; strategy was to inline everything else.  See
;;; feedback-vertex-set.scm for discussion and implementation.  This
;;; is mentioned because the feedback vertex set concept is a useful
;;; one to remember, and the code is still around.  Note that the
;;; implementation of the threshold approach contains a clause
;;; preventing the inlining of procedures that call themselves.

;;; The definition map is a facility for looking up the full
;;; definition form for a FOL procedure given its name.

(define (definition-map program)
  (define defn-map
    (alist->eq-hash-table
     (map (lambda (defn)
            (cons (definiendum defn) defn))
          (filter definition? program))))
  (lambda (name)
    (hash-table/get defn-map name #f)))

