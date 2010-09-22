(define-structure
  ad-environment
  bindings
  parent)

(define (ad-lookup symbol env epsilon)
  (cond ((null? env)
	 (error "Unbound ad-symbol" symbol))
	((environment? env) ; Indirection to Scheme
	 (scheme-value->perturbed-eval-value (eval symbol env) epsilon))
	((ad-environment? env)
	 (let ((binding (assq symbol (ad-environment-bindings env))))
	   (if binding
	       (cdr binding)
	       (ad-lookup symbol (ad-environment-parent env) epsilon))))))

(define (ad-extend-environment env formals arguments)
  (make-ad-environment
   (map cons formals arguments)
   env))

;;; This AD global environment has no initial bindings, and indirects
;;; to the enclosing Scheme environment for lookups.
(define (make-ad-global-environment)
  (make-ad-environment
   `((j* . ,j*))
   (nearest-repl/environment)))

(define (make-ad-user-environment)
  (make-ad-environment
   '()
   (make-ad-global-environment)))
