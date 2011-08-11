(declare (usual-integrations))

;;; Error system.

;;; For now these are just hooks for a more elaborate error system, but
;;; you should respect them.  In particular, 
;;; - use internal-error when reporting a situation that should never
;;;   occur regardless of the program being analyzed
;;; - use syntax-error to report that a putative DVL program is grossly
;;;   malformed
;;; - use dvl-error when flow-analysis discovers an error in a
;;;   well-formed DVL program

(define internal-error error)

(define syntax-error error)

(define (dvl-error message . irritants)
  (apply error message *on-behalf-of* irritants))
