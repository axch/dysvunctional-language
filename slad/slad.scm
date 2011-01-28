(define (slad-eval form env)
  (cond ((constant? form)
	 (constant-value form))
	((variable? form)
	 (lookup form env))
	((pair-form? form)
	 (make-slad-pair (slad-eval (car-subform form) env)
			 (slad-eval (cdr-subform form) env)))
	((lambda-form? form)
	 (make-slad-closure (lambda-formal form)
			    (lambda-body form)
			    env))
	((application? form)
	 (slad-apply (slad-eval (operator-subform form) env)
		     (slad-eval (operand-subform form) env)))
	(else
	 (error "Invalid expression type" form env))))

(define (slad-apply proc arg)
  (cond ((slad-closure? proc)
	 (slad-eval (slad-closure-body proc)
		    (extend-env (slad-closure-formal proc)
				arg
				(slad-closure-env proc))))
	((slad-primitive? proc)
	 ((slad-primitive-implementation proc) arg))
	(else
	 (error "Invalid procedure type" proc arg))))

(define (slad-prepare form)
  (let ((slad-stdlib (with-input-from-file "stdlib.slad" read)))
    (let loop ((tail-form slad-stdlib))
      (if (equal? tail-form "HERE")
	  form
	  `(,@(except-last-pair tail-form)
	    ,(loop (car (last-pair tail-form))))))))

(define (slad-do form)
  (slad-eval (macroexpand (slad-prepare form)) (initial-slad-user-env)))

;;; ----------------------------------------------------------------------
;;;                             Forward Mode
;;; ----------------------------------------------------------------------

;;; The invariant on nested bundles is the outermost bundle
;;; corresponds to the dynamically nearest call to derviative (and
;;; company).  In other words, in (derviative_1 (derviative_2 (lambda (x) ... x ...)))
;;; the reference to x will see (bundle_2 (bundle_1 foo bar) (bundle_1 baz quux)).

(define (transform-and-perturb object perturbation)
  ;; Assume the perturbation is an object of exactly the same type and
  ;; shape as the original object.
  (cond ((forward-transform-known? object)
	 ((get-forward-transform object) perturbation))
	;; ((slad-closure? object)
	;;  (make-slad-closure
	;;   (slad-closure-formal object)
	;;   (slad-closure-body object)    ; Forward mode does not require code changes
	;; 				; except maybe to avoid confusing perturbations
	;;   (transform-and-perturb (slad-closure-env object)
	;; 			 (slad-closure-env perturbation))))
	((slad-primitive? object)
	 (error "Cannot transform primitives whose transforms are not known" object perturbation))
	((slad-real? object)
	 (make-slad-bundle object perturbation))
	((slad-bundle? object)
	 ;; TODO Which way? Cons or interleave? This has to agree with
	 ;; the access pattern to avoid perturbation confusion.
	 (make-slad-bundle object perturbation)
	 (make-slad-bundle (transform-and-perturb (slad-primal object) (slad-primal perturbation))
			   (transform-and-perturb (slad-tangent object) (slad-tangent perturbation))))
	(else
	 (slad-map transform-and-perturb object perturbation))))

(define (zero object)
  (cond ((slad-real? object)
	 0)
	(else
	 (slad-map zero object))))

(define forward-transforms (make-eq-hash-table))

(define (set-forward-transform! object transform)
  ;; All standard forward transforms ignore the perturbation
  (hash-table/put! forward-transforms object (lambda (perturbation)
					       transform))
  (hash-table/put! primal-cache transform object)
  (hash-table/put! tangent-cache transform object)
  'forward-transform-assigned)

(define (forward-transform-known? object)
  (hash-table/get forward-transforms object #f))

(define (get-forward-transform object)
  (hash-table/get forward-transforms object #f))

(define primal-cache (make-eq-hash-table))
(define (primal-cached? object)
  (hash-table/get primal-cache object #f))
(define (cached-primal object)
  (hash-table/get primal-cache object #f))

(define tangent-cache (make-eq-hash-table))
(define (tangent-cached? object)
  (hash-table/get tangent-cache object #f))
(define (cached-tangent object)
  (hash-table/get tangent-cache object #f))

(define (slad-primal thing)
  (cond ((slad-bundle? thing)
	 (slad-bundle-primal thing))
	((primal-cached? thing)
	 (cached-primal thing))
	((slad-real? thing)
	 (error "Cannot take the primal of a non-bundle" thing))
	(else
	 (slad-map slad-primal thing))))

(define (slad-tangent thing)
  (cond ((slad-bundle? thing)
	 (slad-bundle-tangent thing))
	((tangent-cached? thing)
	 (cached-tangent thing))
	((slad-real? thing)
	 (error "Cannot take the tangent of a non-bundle" thing))
	(else
	 (slad-map slad-tangent thing))))

