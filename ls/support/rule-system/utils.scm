(declare (usual-integrations))

;;; This procedure was dredged from the dark recesses of Edwin.  Many
;;; computer scientists would claim that it should never have been
;;; allowed to see the light of day.

(define (procedure-argl proc #!optional default-argl)
  "Returns the arg list of PROC.
   Grumbles if PROC is an undocumented primitive."
  (if (primitive-procedure? proc)
      (let ((doc-string
	     (primitive-procedure-documentation proc)))
	(if doc-string
	    (let ((newline
		   (string-find-next-char doc-string #\newline)))
	      (if newline
		  (string-head doc-string newline)
		  doc-string))
	    (string-append
	     (write-to-string proc)
	     " has no documentation string.")))
      (let ((code (procedure-lambda proc)))
	(if code
	    (lambda-components* code
	      (lambda (name required optional rest body)
		name body
		(append required
		 (if (null? optional) '() `(#!OPTIONAL ,@optional))
		 (if rest `(#!REST ,rest) '()))))
	    (if (default-object? default-argl)
		"No debugging information available for this procedure."
		default-argl)))))

;;; Magic!

(define (make-lambda bvl use-env generate-body)
  (capture-syntactic-environment
   (lambda (transform-env)
     (close-syntax
      `(,(close-syntax 'lambda transform-env)
	,bvl
	,(capture-syntactic-environment
	  (lambda (use-env*)
	    (close-syntax (generate-body use-env*)
			  transform-env))))
      use-env))))
