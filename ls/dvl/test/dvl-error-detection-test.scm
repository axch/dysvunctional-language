(declare (usual-integrations))

(define (expect-dvl-error thunk)
  (let ((result (ignore-errors thunk)))
    (cond ((dvl-error? result) 'ok)
          ((and (condition? result)
                (condition/error? result))
           (error result))
          (else
           (test-fail
            (messagify
             (ensure-forced
              (build-message
               "Expected a DVL error, got" '("<" ">") result))))))))

(in-test-group
 dvl-error-detection

 (define-test (smoke)
   (expect-dvl-error
    (lambda ()
      (analyze-and-generate
       '(1 2))))))
