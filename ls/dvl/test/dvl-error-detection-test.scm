(declare (usual-integrations))

(define (expect-dvl-error thunk)
  (let ((result (ignore-errors thunk)))
    (cond ((dvl-error? result)
           (if (dvl-error-binding result)
               'ok
               (test-fail
                (messagify
                 (ensure-forced
                  (build-message
                   "A DVL error was signaled not on behalf of any binding"
                   '("<" ">") result))))))
          ((and (condition? result)
                (condition/error? result))
           (error result))
          (else
           (test-fail
            (messagify
             (ensure-forced
              (build-message
               "Expected a DVL error, got" '("<" ">") result))))))))

(define-syntax broken-programs
  (syntax-rules ()
    ((_ (test-name program) ...)
     (begin
       (define-test (test-name)
         (expect-dvl-error
          (lambda ()
            (check-program-types
             (compile-to-raw-fol 'program))))) ...))))

(in-test-group
 dvl-error-detection

 (define-test (smoke)
   (expect-dvl-error
    (lambda ()
      (analyze-and-generate
       '(1 2)))))

 (broken-programs
  (well-typed-*       (* sin 3))
  (no-ternary-*       (* 1 2 3))
  (no-ternary-*-2     (* (real 1) 2 3))
  (no-ternary-*-3     (* 1 (real 2) 3))
  (well-typed-sin     (sin sin))
  (well-typed-real-declaration (real sin))
  (well-typed-gensym= (gensym= 1 2))
  (well-typed-gensym= (gensym= (gensym) 2))
  (well-typed-gensym= (gensym= 1 (gensym)))
  ))
