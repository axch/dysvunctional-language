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

(define (report-dvl-error dvl-error port)
  (format-error-message
   (dvl-error-message dvl-error)
   `(,(error-irritant/noise " in")
     ,(dvl-error-binding dvl-error)
     ,(error-irritant/noise ":")
     ,@(dvl-error-irritants dvl-error))
   port))

(define condition-type:dvl-error
  (make-condition-type
   'dvl-error condition-type:simple-error '(binding) report-dvl-error))

(define make-dvl-error
  (condition-constructor condition-type:dvl-error '(message binding irritants)))
(define dvl-error? (condition-predicate condition-type:dvl-error))
(define dvl-error-message (condition-accessor condition-type:dvl-error 'message))
(define dvl-error-binding (condition-accessor condition-type:dvl-error 'binding))
(define dvl-error-irritants (condition-accessor condition-type:dvl-error 'irritants))
(define signal-dvl-error
  (condition-signaller
   condition-type:dvl-error '(message binding irritants) standard-error-handler))

(define (dvl-error message . irritants)
  (signal-dvl-error message *on-behalf-of* irritants))

;;; Inspecting and debugging tools

(define (run-up-binding-chain binding stop?)
  (cond ((not (= 1 (length (binding-notify binding))))
         binding)
        ((stop? (car (binding-notify binding)))
         binding)
        (else (run-up-binding-chain (car (binding-notify binding)) stop?))))

(define (run-up-eval-binding-chain binding)
  (run-up-binding-chain binding apply-binding?))

(define (confusing-apply-binding? binding)
  (and (apply-binding? binding)
       (or (not (= 1 (length (binding-notify binding))))
           (let* ((predecessor (car (binding-notify binding)))
                  (pred-exp (binding-exp predecessor))
                  (pred-operator (car pred-exp)))
             (not (or (eq? 'if-procedure pred-operator)
                      (lambda-form? pred-operator)))))))
