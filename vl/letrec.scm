(declare (usual-integrations))

(define (equivalence-classes equiv? items)
  (if (null? items)
      '()
      (let* ((item (first items))
	     (classes (equivalence-classes equiv? (cdr items)))
	     (class-of-item
	      (find (lambda (class)
		      ;; Assumes equiv? is transitive
		      (equiv? item (car class)))
		    classes)))
	(if class-of-item
	    (cons (cons item class-of-item)
		  (delq class-of-item classes))
	    (cons (list item) classes)))))

(define (topological-sort < items)
  (if (null? items)
      '()
      (let ((min (find
		  (lambda (x)
		    (not (any (lambda (y)
				(and (not (eq? x y))
				     (< y x)))
			      items)))
		  items)))
	(if (not min)
	    (error "Purported order contains a cycle" < items)
	    (cons min (topological-sort < (delq min items)))))))

;;; A graph is an alist of node with its adjacency list.  Nodes are
;;; assumed to be normalized to be eq?

(define (neighbors node graph)
  (cdr (assq node graph)))

(define (transitive-closure graph)
  (define (node-union nodes1 nodes2)
    (lset-union eq? nodes1 nodes2))
  (define (same-node-set? nodes1 nodes2)
    (lset= eq? nodes1 nodes2))
  (let loop ((graph graph))
    (define (second-neighbors nodes)
      (reduce node-union '()
	      (map (lambda (node)
		     (neighbors node graph))
		   nodes)))
    (define (new-neighbors node.neighbors)
      (let ((node (car node.neighbors))
	    (neighbors (cdr node.neighbors)))
	(cons node (node-union neighbors (second-neighbors neighbors)))))
    (let ((new-graph (map new-neighbors graph)))
      (if (every same-node-set? (map cdr graph) (map cdr new-graph))
	  graph
	  (loop new-graph)))))

(define (strongly-connected-components transitive-graph)
  (equivalence-classes
   (lambda (node1 node2)
     (and (memq node2 (neighbors node1 transitive-graph))
	  (memq node1 (neighbors node2 transitive-graph))))
   (map car transitive-graph)))

(define (normalize-graph graph =)
  (let ((nodes (map car graph)))
    (map (lambda (node.neighbors)
	   (cons (car node.neighbors)
		 (map (lambda (neighbor)
			(car (member neighbor nodes =)))
		      (cdr node.neighbors))))
	 graph)))

(define (reference-graph variables expressions)
  (normalize-graph
   (map (lambda (variable expression)
	  (cons variable (lset-intersection equal?
			  variables (free-variables (macroexpand expression)))))
	variables
	expressions)
   equal?))

(define (lightweight-letrec-conversion form)
  (let* ((bindings (cadr form))
	 (variables (map car bindings))
	 (expressions (map cadr bindings))
	 (body (cddr form)))
    (let ((references
	   (transitive-closure (reference-graph variables expressions))))
      ;; TODO I could sweep out unreferenced variables with
      #;
      (filter (lambda (var.neighbors)
		(any (lambda (body-var)
		       (memq (car var.neighbors) (referees body-var)))
		     (free-variables (macroexpand body))))
	      references)
      ;; This may be dangerous if those variables' expressions are to
      ;; be executed for effect.
      (define (referees var) (neighbors var references))
      (define (referenced-by? component1 component2)
	(any (lambda (var2)
	       (any (lambda (var1)
		      (memq var1 (referees var2)))
		    component1))
	     component2))
      (let loop ((clusters
		  (topological-sort
		   referenced-by?
		   (strongly-connected-components references))))
	(cond ((null? clusters)
	       `(let ()
		  ,@body))
	      (else
	       (let ((cluster (car clusters)))
		 (if (and (null? (cdr cluster))
			  (not (memq (car cluster) (referees (car cluster)))))
		     (let* ((var (car cluster))
			    (binding (assq var bindings)))
		       `(let (,binding)
			  ,(loop (cdr clusters))))
		     (let ((bindings (map (lambda (var)
					    (assq var bindings))
					  cluster)))
		       `(raw-letrec ,bindings
			  ,(loop (cdr clusters))))))))))))

(define-exp-macro! 'letrec lightweight-letrec-conversion)
(define-exp-macro! 'raw-letrec letrec-transformer)
