;;;;           Most General Generic-Operator Dispatch

(declare (usual-integrations))

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by ASSIGN-OPERATION).

;;; To be the correct branch all arguments must be accepted by the
;;; branch predicates, so this makes it necessary to backtrack to find
;;; another branch where the first argument is accepted if the second
;;; argument is rejected.  Here backtracking is implemented by success
;;; and failure continuation procedures.

(define (make-generic-operator arity default-operation)
  (let ((record (make-operator-record arity)))

    (define (operator . arguments)
      (if (not (= (length arguments) arity))
          (error:wrong-number-of-arguments operator arity arguments))
      (let ((succeed
	     (lambda (handler)
	       (apply handler arguments))))
	(let per-arg
	    ((tree (operator-record-tree record))
	     (args arguments)
	     (fail
	      (lambda ()
		(error:no-applicable-methods operator arguments))))
	  (let per-pred ((tree tree) (fail fail))
	    (cond ((pair? tree)
		   (if ((caar tree) (car args))
		       (if (pair? (cdr args))
			   (per-arg (cdar tree)
				    (cdr args)
				    (lambda ()
				      (per-pred (cdr tree) fail)))
			   (succeed (cdar tree)))
		       (per-pred (cdr tree) fail)))
		  ((null? tree)
		   (fail))
		  (else
		   (succeed tree)))))))

    (hash-table/put! *generic-operator-table* operator record)
    (if default-operation
	(assign-operation operator default-operation))
    operator))

(define *generic-operator-table*
  (make-eq-hash-table))

(define (make-operator-record arity) (cons arity '()))
(define (operator-record-arity record) (car record))
(define (operator-record-tree record) (cdr record))
(define (set-operator-record-tree! record tree) (set-cdr! record tree))

(define (assign-operation operator handler . argument-predicates)
  (let ((record
         (let ((record (hash-table/get *generic-operator-table* operator #f))
               (arity (length argument-predicates)))
           (if record
               (begin
                 (if (not (<= arity (operator-record-arity record)))
                     (error "Incorrect operator arity:" operator))
                 record)
               (let ((record (make-operator-record arity)))
                 (hash-table/put! *generic-operator-table* operator record)
                 record)))))
    (set-operator-record-tree! record
                               (bind-in-tree argument-predicates
                                             handler
                                             (operator-record-tree record))))
  operator)

(define defhandler assign-operation)

(define (bind-in-tree keys handler tree)
  (let loop ((keys keys) (tree tree))
    (if (pair? keys)
	(let find-key ((tree* tree))
	  (if (pair? tree*)
	      (if (eq? (caar tree*) (car keys))
		  (begin
		    (set-cdr! (car tree*)
			      (loop (cdr keys) (cdar tree*)))
		    tree)
		  (find-key (cdr tree*)))
	      (cons (cons (car keys)
			  (loop (cdr keys) '()))
		    tree)))
	(if (pair? tree)
	    (let ((p (last-pair tree)))
	      (if (not (null? (cdr p)))
		  (warn "Replacing a handler:" (cdr p) handler))
	      (set-cdr! p handler)
	      tree)
	    (begin
	      (if (not (null? tree))
		  (warn "Replacing top-level handler:" tree handler))
	      handler)))))