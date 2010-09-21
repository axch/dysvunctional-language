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

;;; This defines the representation of ad-eval objects as Scheme
;;; objects and vice versa.  At the moment, that representation is not
;;; very complicated.
(define (scheme-value->ad-eval-value x)
  (if (procedure? x)
      (make-ad-primitive x)
      x))
