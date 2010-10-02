;;;; Traditional LISP property lists
;;;   extended to work on any kind of 
;;;   eq? data structure.

;;; Property lists are a way of creating data that
;;; looks like a record structure without
;;; commiting to the fields that will be used
;;; until run time.  The use of such flexible
;;; structures is frowned upon by most computer
;;; scientists, because it is hard to statically
;;; determine the bounds of the behavior of a
;;; program written using this stuff.  But it
;;; makes it easy to write programs that confuse
;;; such computer scientists.  I personally find
;;; it difficult to write without such crutches.
;;; -- GJS

(declare (usual-integrations))

(define eq-properties (make-eq-hash-table))

(define (eq-label! node . plist)
  (let loop ((plist plist))
    (cond ((null? plist) node)
	  ((null? (cdr plist)) (error "Malformed plist"))
	  (else
	   (eq-put! node (car plist) (cadr plist))
	   (loop (cddr plist))))))

(define (eq-put! node property value)
  (let ((plist
	 (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (set-cdr! vcell value)
	  (hash-table/put! eq-properties node
	    (cons (cons property value) plist)))))
  node)

(define (eq-get node property)
  (let ((plist
	 (hash-table/get eq-properties node '())))
    (let ((vcell (assq property plist)))
      (if vcell
	  (cdr vcell)
	  #f))))

(define (eq-rem! node . properties)
  (for-each
   (lambda (property)
     (let ((plist
	    (hash-table/get eq-properties node '())))
       (let ((vcell (assq property plist)))
	 (if vcell
	     (hash-table/put! eq-properties node
			      (delq! vcell plist))))))
   properties)
  node)


(define (eq-adjoin! node property new)
  (eq-put! node property
	   (lset-adjoin eq? (or (eq-get node property) '()) new))
  node)

(define (eq-plist node)
  (let ((plist
	 (hash-table/get eq-properties node #f)))
    (if plist (cons node plist) #f)))

(define (eq-clone! source target)
  (hash-table/put! eq-properties target
    (hash-table/get eq-properties source '())))

;;; Path names are built with properties.

(define (eq-path path)
  (define (lp node)
    (if node
	(if (pair? path)
	    (eq-get ((eq-path (cdr path)) node)
		    (car path))
	    node)
	#f))
  lp)
