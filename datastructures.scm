(define-structure (ad-compound (safe-accessors #t))
  body
  env
  formals)

;;; AD-primitive procedures are implemented as Scheme procedures that
;;; accept the AD-eval objects that are their arguments and return the
;;; AD-eval object that is the result.  Any representation conversions
;;; need to be done elsewhere.
(define-structure (ad-primitive (safe-accessors #t))
  implementation
  made-from)

(define (ad-procedure? thing)
  (or (ad-primitive? thing)
      (ad-compound? thing)))

;;; This defines the representation of ad-eval objects as Scheme
;;; objects and vice versa.  At the moment, that representation is not
;;; very complicated.
(define (scheme-value->ad-eval-value x)
  (if (procedure? x)
      (make-ad-primitive
       (lambda args
	 (apply x (map ad-eval-value->scheme-value args)))
       x)
      x))

(define (ad-eval-value->scheme-value x)
  (if (ad-procedure? x)
      (lambda args
	(ad-apply x (map scheme-value->ad-eval-value args)))
      x))

(define-structure (perturbation-type (safe-accessors #t))
  dual
  primal
  perturbation
  primal?
  parent)

(define the-non-perturbation (make-perturbation-type #f #f #f #f #f))

(define (non-perturbation? thing)
  (eq? thing the-non-perturbation))

(define perturbation-type-count 0)

(define-structure (perturbed (safe-accessors #t))
  primal
  perturbation
  count)

(define (gen-primal count)
  (lambda (perturbed)
    (if (= count (perturbed-count perturbed))
	(perturbed-primal perturbed)
	(error "Wrong type of perturbation" perturbed count))))

(define (gen-perturbation count)
  (lambda (perturbed)
    (if (= count (perturbed-count perturbed))
	(perturbed-perturbation perturbed)
	(error "Wrong type of perturbation" perturbed count))))

(define (gen-primal? count)
  (lambda (thing)
    (not (and (perturbed? thing)
	      (= count (perturbed-count thing))))))

(define (gen-make-dual count)
  (lambda (primal perturbation)
    (make-perturbed primal perturbation count)))

(define (make-fresh-perturbation-type parent)
  (set! perturbation-type-count (+ perturbation-type-count 1))
  (make-perturbation-type
   (gen-make-dual perturbation-type-count)
   (gen-primal perturbation-type-count)
   (gen-perturbation perturbation-type-count)
   (gen-primal? perturbation-type-count)
   parent))

(define (primal* thing)
  (if (perturbed? thing)
      (primal* (perturbed-primal thing))
      thing))

(define j* (list 'j*))

(define (j*? thing)
  (eq? j* thing))

(define (jacobian procedure)
  (let ((candidate
	 (assq (ad-primitive-made-from procedure)
	       `((,sin . ,cos)
		 (,*   . ,(lambda (x y)
			    (list y x)))))))
    (if candidate
	(cdr candidate)
	(error "Unsupported primitive primal procedure" procedure))))
