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

