(declare (usual-integrations))
;;;; Closures

(define-structure
  (closure
   safe-accessors
   (constructor %make-closure)
   (print-procedure
    (simple-unparser-method 'closure
     (lambda (closure)
       (list (closure-exp closure)
	     (closure-env closure))))))
  exp
  free-variables
  env)

(define (closure-formal closure)
  (lambda-formal (closure-exp closure)))

(define (closure-body closure)
  (lambda-body (closure-exp closure)))

(define (env-slice env variables)
  (make-env
   (filter (lambda (binding)
	     (member (car binding) variables))
	   (env-bindings env))))

;;; To keep environments in canonical form, closures only keep the
;;; variables they want.
(define (make-closure exp env)
  (let ((free (free-variables exp)))
    (%make-closure exp free (env-slice env free))))

(define (congruent-map f object1 object2 lose)
  (cond ((and (closure? object1) (closure? object2)
	      (equal? (closure-exp object1) (closure-exp object2)))
	 (make-closure
	  (closure-exp object1)
	  (f (closure-env object1) (closure-env object2))))
	((and (env? object1) (env? object2))
	 (congruent-env-map f object1 object2 lose))
	((and (pair? object1) (pair? object2))
	 (cons (f (car object1) (car object2))
	       (f (cdr object1) (cdr object2))))
	(else
	 (lose))))

(define (object-reduce reducer object)
  (cond ((closure? object)
	 (reducer (list (closure-env object))))
	((env? object)
	 (reducer (map cdr (env-bindings object))))
	((pair? object)
	 (reducer (list (car object) (cdr object))))
	(else
	 (reducer '()))))

(define (congruent-reduce reducer object1 object2 lose)
  (cond ((and (closure? object1) (closure? object2)
	      (equal? (closure-exp object1) (closure-exp object2)))
	 (reducer (list (closure-env object1))
		  (list (closure-env object2))))
	((and (env? object1) (env? object2)
	      (equal? (map car (env-bindings object1))
		      (map car (env-bindings object2))))
	 (reducer (map cdr (env-bindings object1))
		  (map cdr (env-bindings object2))))
	((and (pair? object1) (pair? object2))
	 (reducer (list (car object1) (cdr object1))
		  (list (car object2) (cdr object2))))
	(else
	 (lose))))

;;;; Free variables

(define (free-variables exp)
  (cond ((variable? exp) (list exp))
	((lambda-form? exp)
	 (lset-difference equal? (free-variables (lambda-body exp))
			  (free-variables (lambda-formal exp))))
	((pair-form? exp)
	 (lset-union equal? (free-variables (car-subform exp))
		     (free-variables (cdr-subform exp))))
	((pair? exp) ; Not application? to handle formals lists
	 (lset-union equal? (free-variables (car exp))
		     (free-variables (cdr exp))))
	(else '())))
