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

(define *epsilon-count* 0)

(define the-non-perturbation 0)

(define (non-perturbation? thing)
  (= thing 0))

(define-structure (perturbed (safe-accessors #t))
  primal
  perturbation
  epsilon)

(define perturbed-epsilon
  (let ((perturbed-epsilon perturbed-epsilon))
    (lambda (thing)
      (if (perturbed? thing)
	  (perturbed-epsilon thing)
	  0))))

(define (gen-primal epsilon)
  (lambda (perturbed)
    (cond ((= epsilon (perturbed-epsilon perturbed))
	   (perturbed-primal perturbed))
	  ((> epsilon (perturbed-epsilon perturbed))
	   perturbed)
	  (else
	   (error "Wrong type of perturbation" perturbed epsilon)))))

(define (gen-perturbation epsilon)
  (lambda (perturbed)
    (cond ((= epsilon (perturbed-epsilon perturbed))
	   (perturbed-perturbation perturbed))
	  ((> epsilon (perturbed-epsilon perturbed))
	   0)
	  (else
	   (error "Wrong type of perturbation" perturbed epsilon)))))

(define (gen-make-dual epsilon)
  (lambda (primal perturbation)
    (if (and (number? perturbation)
	     (= 0 perturbation))
	primal
	(make-perturbed primal perturbation epsilon))))

;;; TODO Do I want to turn the epsilons into a skip list?
(define (make-fresh-epsilon parent-epsilon)
  (set! *epsilon-count* (+ *epsilon-count* 1))
  *epsilon-count*)

(define (epsilon-parent epsilon)
  (- epsilon 1))

(define (primal* thing)
  (if (perturbed? thing)
      (primal* (perturbed-primal thing))
      thing))

(define j* (list 'j*))

(define (j*? thing)
  (eq? j* thing))

(define (zero . args)
  (map (lambda (x) 0) args))

(define (jacobian procedure)
  (let ((candidate
	 (assq (ad-primitive-made-from procedure)
	       `((,sin . cos)
		 (,cos . sin)
		 (,*   . (lambda (x y)
			   (list y x)))
		 (,cdr . zero)
		 (,list . zero)
		 (,zero . zero)))))
    (if candidate
	(perturbed-eval (cdr candidate) (make-ad-user-environment) the-non-perturbation)
	(error "Unsupported primitive primal procedure" procedure))))
