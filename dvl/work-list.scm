(declare (usual-integrations))


;; This one is for use during abstract evaluation; it is always "on
;; behalf of" some binding that is therefore assumed to depend on the
;; answer.
(define (analysis-get-in-world exp env world analysis win)
  (if (not *on-behalf-of*)
      (error "analysis-get-in-world must always be done on behalf of some binding"))
  (define (search-win binding)
    (register-notification! binding *on-behalf-of*)
    (world-update-binding binding world win))
  (analysis-search exp env analysis
   search-win
   (lambda ()
     (if (impossible-world? world)
         (win abstract-none impossible-world)
         (let ((binding (make-binding exp env world abstract-none impossible-world)))
           (analysis-new-binding! analysis binding)
           (search-win binding))))))

;; This one is just for querying the analysis, for example during code
;; generation.
(define (analysis-get exp env analysis)
  (analysis-search exp env analysis binding-value (lambda () abstract-none)))

(define *on-behalf-of* #f)

(define (initial-analysis program)
  (make-analysis
   (list
    (make-binding
     program
     (initial-user-env)
     (initial-world)
     abstract-none
     impossible-world))))

(define (step-analysis! analysis)
  (if (null? (analysis-queue analysis))
      #f
      (let ((binding (analysis-queue-pop! analysis)))
        (fluid-let ((*on-behalf-of* binding))
          (let ((exp (binding-exp binding))
                (env (binding-env binding))
                (world (binding-world binding))
                (value (binding-value binding)))
            (let ((answer ; break tail recursion here makes it easier
                          ; to debug errors inside the flow analysis
                   (refine-eval
                    exp env world analysis
                    (lambda (new-value new-world)
                      (let ((new-value (abstract-union value new-value)))
                        (if (abstract-equal? value new-value)
                            #t
                            (begin
                              (set-binding-value! binding new-value)
                              (set-binding-new-world! binding new-world)
                              (for-each (lambda (dependency)
                                          (analysis-notify! analysis dependency))
                                        (binding-notify binding))
                              #t)))))))
              answer))))))

(define *analyze-wallp* #f)

(define (analyze program)
  (let ((analysis (initial-analysis (macroexpand program))))
    (let loop ((continue? #t)
               (count 0))
      (if (and (number? *analyze-wallp*)
               (= 0 (modulo count *analyze-wallp*)))
          (show-analysis analysis))
      (if continue?
          (loop (step-analysis! analysis) (+ count 1))
          (begin
            (if *analyze-wallp*
                (show-analysis analysis))
            analysis)))))
