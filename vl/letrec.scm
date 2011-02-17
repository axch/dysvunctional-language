(declare (usual-integrations))
;;;; LETREC Conversion

;;; Consider a LETREC with n bindings.  The LETREC-TRANSFORMER
;;; provided in macro.scm produces output of size O(n^2); which,
;;; moreover, imposes O(n^2) overhead on interpretation of every call
;;; to procedures defined in such a LETREC.  Given that parallel
;;; definitions are expanded into a big LETREC, and that a library
;;; file may well offer hundreds of parallel definitions (for example,
;;; R5RS defines 80 "library procedures"), this is a serious
;;; performance problem.

;;; Fortunately, the performance problem has a simple solution.  In
;;; practice, those hundreds of library procedures would not all be
;;; mutually recursive; in fact, one would expect the vast majority of
;;; recursions to involve only one or two procedures.  It is therefore
;;; profitable to convert a large LETREC into a collection of LETs and
;;; smaller LETRECs that precisely capture the scoping structure that
;;; is actually required.  This can be achieved with a computation on
;;; the static binding-reference graph of the input LETREC.

;;; Given a LETREC form, consider the following graph.  The vertices
;;; are the names bound by that LETREC form.  There is an edge from
;;; vertex x to vertex y iff y appears free in the expression to which
;;; x is bound.  The strongly connected components of this graph are
;;; exactly the irreducible mutually-recursive clusters bound by the
;;; LETREC.  They can be bound in topological sort order in a series
;;; of separate, hopefully smaller, nested LETREC forms.  As a further
;;; optimization, one can distinguish a singleton component that has a
;;; self-loop (and therefore requires a unary LETREC) from one that
;;; does not (and can therefore be defined with a simple LET).

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

(define (points-to? node1 node2 graph)
  (memq node2 (neighbors node1 graph)))

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
     (and (points-to? node1 node2 transitive-graph)
	  (points-to? node2 node1 transitive-graph)))
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
      ;; I could sweep out unreferenced variables.  This may be
      ;; dangerous if those variables' expressions are to be executed
      ;; for effect; but why would anyone use a letrec for that?
      #;
      (set! references
	    (let ((entry-points (lset-intersection equal?
				 variables (free-variables (macroexpand-body body)))))
	     (filter (lambda (var.neighbors)
		       (any (lambda (body-var)
			      (or (equal? (car var.neighbors) body-var)
				  (points-to? body-var (car var.neighbors) references)))
			    entry-points))
		     references)))
      (define (referenced-by? component1 component2)
	(any (lambda (var2)
	       (any (lambda (var1)
		      (points-to? var2 var1 references))
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
			  (not (points-to? (car cluster) (car cluster) references)))
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
