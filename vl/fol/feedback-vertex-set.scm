(declare (usual-integrations))

;;; A feedback vertex set of a directed graph G is a subset of the
;;; vertices of G whose removal causes G to become acyclic.  It is
;;; usually desirable for such feedback sets to be smaller rather than
;;; larger; however, the problem of finding a minimum feedback vertex
;;; set for a directed graph is known to be NP-complete.  In the
;;; present application, minimality is not strictly necessary, whereas
;;; finding a plausible acceptable solution quickly is also important.
;;; To that end, this code heuristically spits out some feedback
;;; vertex set of the given graph.

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
             ((memq node (out-neighbors node))
              (feedback! node))))
     (hash-table/datum-list node-map))
    (let ((nodes-left (hash-table/count node-map)))
      (cond ((= 0 nodes-left)
             (map name feedback))
            ((= old-nodes-left nodes-left)
             ;; If pruning stalls, heuristically remove a node
             (feedback! (select))
             (prune (- nodes-left 1)))
            (else
             (prune nodes-left))))))
