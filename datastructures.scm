(define-structure
  ad-compound
  body
  env
  formals)

;;; AD-primitive procedures are implemented as Scheme procedures that
;;; accept the AD-eval objects that are their arguments and return the
;;; AD-eval object that is the result.  Any representation conversions
;;; need to be done elsewhere.
(define-structure
  ad-primitive
  implementation)

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
	 (apply x (map ad-eval-value->scheme-value args))))
      x))

(define (ad-eval-value->scheme-value x)
  (if (ad-procedure? x)
      (lambda args
	(ad-apply x (map scheme-value->ad-eval-value args)))
      x))
