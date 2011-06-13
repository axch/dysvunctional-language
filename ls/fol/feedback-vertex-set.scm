(declare (usual-integrations))
;;;; Computing feedback vertex sets

;;; A feedback vertex set of a directed graph G is a subset of the
;;; vertices of G whose removal causes G to become acyclic.  It is
;;; usually desirable for such feedback sets to be smaller rather than
;;; larger; however, the problem of finding a minimum feedback vertex
;;; set for a directed graph is known to be NP-complete.  Furthermore,
;;; no good approximation algorithms are known.  In the present
;;; application, minimality is not strictly necessary, whereas finding
;;; a plausible acceptable solution quickly is important.  To that
;;; end, this code heuristically spits out some feedback vertex set of
;;; the given graph.

;;; The incoming graph is accepted as an alist of vertex with
;;; out-neighbors.  It is assumed normalized so that sameness of
;;; vertices can be tested with eq?.

(define (feedback-vertex-set graph)
  ;; The internal graph representation is a linked record pile, with
  ;; one record per vertex maintaining bidirectional adjacency and
  ;; degrees.
  (define-structure (node safe-accessors (conc-name #f))
    name
    out-neighbors
    out-degree
    in-neighbors
    in-degree)
  (define (add-out-neighbor! node1 node2)
    (set-out-neighbors! node1 (cons node2 (out-neighbors node1)))
    (set-out-degree! node1 (+ (out-degree node1) 1)))
  (define (add-in-neighbor! node1 node2)
    (set-in-neighbors! node1 (cons node2 (in-neighbors node1)))
    (set-in-degree! node1 (+ (in-degree node1) 1)))
  (define (attach-edge! node1 node2)
    (add-out-neighbor! node1 node2)
    (add-in-neighbor! node2 node1))
  (define (delete-node! node)
    (hash-table/remove! node-map (name node))
    (for-each
     (lambda (out-neighbor)
       (set-in-neighbors! out-neighbor (delq node (in-neighbors out-neighbor)))
       (set-in-degree! out-neighbor (- (in-degree out-neighbor) 1)))
     (out-neighbors node))
    (for-each
     (lambda (in-neighbor)
       (set-out-neighbors! in-neighbor (delq node (out-neighbors in-neighbor)))
       (set-out-degree! in-neighbor (- (out-degree in-neighbor) 1)))
     (in-neighbors node)))

  ;; Define and initialize a structure mapping the incoming vertices
  ;; to their nodes.
  (define node-map (make-eq-hash-table))
  (define (vertex-node vertex)
    (hash-table/get node-map vertex #f))
  (for-each (lambda (vertex)
              (hash-table/put! node-map vertex (make-node vertex '() 0 '() 0)))
            (map car graph))
  (for-each (lambda (vertex.neighbors)
              (let ((vertex (vertex-node (car vertex.neighbors)))
                    (neighbors (map vertex-node (cdr vertex.neighbors))))
                (for-each (lambda (neighbor)
                            (attach-edge! vertex neighbor))
                          neighbors)))
            graph)

  ;; The algorithm proper iteratively prunes the graph, removing
  ;; vertices that clearly cannot be in a minimal feedback vertex set
  ;; or those that clearly must be in every feedback vertex set.
  ;; Should the pruning stall, it heuristically selects a vertex and
  ;; decides that it will be part of the feedback vertex set, then
  ;; removes that vertex and resumes pruning.

  (define feedback '())
  (define (non-feedback! node)
    (delete-node! node))
  (define (feedback! node)
    (set! feedback (cons node feedback))
    (delete-node! node))

  (define (select)
    (define (node-max node1 node2)
      (cond ((not node2) node1)
            ((> (* (in-degree node1) (out-degree node1))
                (* (in-degree node2) (out-degree node2)))
             node1)
            (else node2)))
    (fold node-max #f (filter-map vertex-node (map car graph))))

  (let prune ((old-nodes-left (hash-table/count node-map)))
    (for-each
     (lambda (node)
       (cond ((= 0 (out-degree node))
              (non-feedback! node))
             ((= 0 (in-degree node))
              (non-feedback! node))
             ((memq node (out-neighbors node)) ; Self loop
              (feedback! node))))
     (hash-table/datum-list node-map))
    (let ((nodes-left (hash-table/count node-map)))
      (cond ((= 0 nodes-left) ; Done
             (map name feedback))
            ((= old-nodes-left nodes-left)
             ;; If pruning stalls, heuristically remove a node
             (feedback! (select))
             (prune (- nodes-left 1)))
            (else
             (prune nodes-left))))))

;;; A note on idempotence.  In this code, feedback vertex sets are
;;; used in the context of inlining procedures (and, when support for
;;; union types is added, this code will also be needed for SRA).
;;; Specifically, one must not inline all the procedures in any cycle
;;; of the (static) call graph; so one computes that graph's feedback
;;; vertex set and inlines all procedures that are not in it.  As a
;;; graph operation, that consists of iteratively removing any
;;; non-feedback vertex and multiplying out all edges that went into
;;; it by all edges that went out of it.  One might ask whether this
;;; operation is idempotent, or whether it might produce a graph that
;;; may still have a non-trivial feedback vertex set.

;;; To answer that question, consider when will a graph's feedback
;;; vertex set contain all vertices in the graph.  Clearly, if all
;;; vertices have self-loop edges, they will all be in the feedback
;;; set.  What if not?  The above code will begin by pruning all
;;; vertices that do have self-loops and putting them in the feedback
;;; set.  If there is anything left, a process of heuristic selection
;;; and deletion will ensue.  If it ever produces a vertex with zero
;;; in-degree or zero out-degree, that vertex will not be in the
;;; feedback set, and the feedback set will not be all the vertices in
;;; the graph.  Otherwise, consider the last vertex that remains
;;; unselected.  It has no other vertices left to which to send edges,
;;; so if it is to still have positive in- and out-degree after
;;; removal of selected vertices, it must be a self-loop.  But the
;;; heuristic selection process cannot introduce a new self-loop, and
;;; all self-loops were swept out before it started, so this situation
;;; is impossible.  Therefore, the only graphs whose feedback vertex
;;; set is their whole vertex set are those all of whose vertices have
;;; self-loops.

;;; In order for an inlining-type operation to be idempotent, then,
;;; the result of one application of it will need to have all vertices
;;; have self-loops.  For this feedback vertex set algorithm, that is
;;; not true.  Consider the feedback set of the following graph G:
;;;
;;;      /-----\
;;;      V     |
;;;      A---->B
;;;      ^    /
;;;       \  /
;;;        \V
;;;         C
;;;         ^\
;;;        /  \
;;;       /    V
;;;      D<----E
;;;      |     ^
;;;      \-----/
;;;
;;; G has no vertices of in- or out- degree 0 and no vertices with
;;; self-loops, so the first thing that FEEDBACK-VERTEX-SET will do is
;;; heuristically select a node to put in the feedback set.  The
;;; heuristic is such that in this case it will choose node C.  After
;;; that G splits into two loops of size two, one vertex from each of
;;; which (without loss of generality, A and D) will also be selected.
;;; After inlining B and E, C will not have a self-loop in the
;;; resulting graph, so on the initial graph G the inlining operation
;;; is not idempotent.
