(declare (usual-integrations))

;;; lifting lets needs alpha
;;;   informs inlining direct constructions
;;; inlining singletons needs alpha
;;;   informs pushing accessors
;;;     (let ((x (if ... (cons) (cons))))
;;;       (car x))
;;;   informs inlining direct constructions by let-lifting
;;;   may need to be undone by inlining direct constructions
;;; inlining direct constructions
;;;   informs pushing accessors
;;;   informs itself by let-lifting
;;; pushing accessors makes the code smaller
;;;   informs deletion of variables
;;;   informs inlining singletons
;;; deletion of variables and empty lets makes the code smaller
;;;   informs inlining direct constructions by let-lifting
;;;   informs itself
;;;   informs inlining singletons

;;; delete dead code -> alpha rename -> lift lets -> 
;;; inline direct -> push -> delete -> loop? inline singletons? ->

;; Empirically, this seems to give a reduction of about 20% of pairs
;; when given (inline (structure-definitions->vectors raw-fol)).
(define (intraprocedural-dead-elimination expr)
  (define (no-used-vars) '())
  (define (single-used-var var) (list var))
  (define (union vars1 vars2)
    (lset-union eq? vars1 vars2))
  (define (difference vars1 vars2)
    (lset-difference eq? vars1 vars2))
  (define used? memq)
  (let loop ((expr expr)
             (win (lambda (new-expr used-vars) new-expr)))
    (define (loop* exprs win)
      (let loop* ((exprs exprs)
                  (finished '())
                  (used (no-used-vars)))
        (if (null? exprs)
            (win (reverse finished) used)
            (loop (car exprs)
             (lambda (new-expr expr-used)
               (loop* (cdr exprs) (cons new-expr finished)
                      (union used expr-used)))))))
    (cond ((symbol? expr)
           (win expr (single-used-var expr)))
          ((number? expr)
           (win expr (no-used-vars)))
          ((if-form? expr)
           (let ((predicate (cadr expr))
                 (consequent (caddr expr))
                 (alternate (cadddr expr)))
             (loop predicate
              (lambda (new-predicate pred-used)
                (loop consequent
                 (lambda (new-consequent cons-used)
                   (loop alternate
                    (lambda (new-alternate alt-used)
                      (win `(if ,new-predicate
                                ,new-consequent
                                ,new-alternate)
                           (union pred-used (union cons-used alt-used)))))))))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 (loop body
                       (lambda (new-body body-used)
                         (let ((new-bindings
                                (filter (lambda (binding)
                                          (used? (car binding) body-used))
                                        bindings)))
                           (loop* (map cadr new-bindings)
                                  (lambda (new-exprs used)
                                    (win (empty-let-rule
                                          `(let ,(map list (map car new-bindings)
                                                      new-exprs)
                                             ,new-body))
                                         (union used (difference
                                                      body-used (map car bindings)))))))))
                 (error "Malformed LET" expr))))
          (else (loop* expr win)))))

;; Empirically, this has no effect without some form of construction
;; inlining.
(define (push-down-accessors expr)
  (define (empty-context) '())
  (define empty-context? null?)
  (define pop cadr)
  (define (accessor? expr)
    (or (cons-ref? expr)
        (vector-ref? expr)))
  (define (cons-ref? expr)
    (and (pair? expr) (pair? (cdr expr)) (null? (cddr expr))
         (memq (car expr) '(car cdr))))
  (define (vector-ref? expr)
    (and (pair? expr) (pair? (cdr expr)) (pair? (cddr expr)) (null? (cdddr expr))
         (eq? (car expr) 'vector-ref) (number? (caddr expr))))
  (define (push-access expr1 expr2)
    `(,(car expr1) ,expr2 ,@(cddr expr1)))
  (define (access context expr)
    (cond ((eq? 'car (car context))
           (cadr expr))
          ((eq? 'cdr (car context))
           (caddr expr))
          ((eq? 'vector-ref (car context))
           (list-ref (cdr expr) (caddr context)))))
  (define (construction? expr)
    (and (pair? expr)
         (memq (car expr) '(cons vector))))
  (define (compatible? context expr)
    (or (and (memq (car context) '(car cdr)) (eq? (car expr) 'cons))
        (and (eq? (car context) 'vector-ref) (eq? (car expr) 'vector))))
  (define (reconstruct context expr)
    (if (empty-context? context)
        expr
        (reconstruct (pop context) (push-access context expr))))
  (let loop ((expr expr) (context (empty-context)))
    (cond ((symbol? expr)
           (reconstruct context expr))
          ((number? expr)
           (reconstruct context expr))
          ((if-form? expr)
           (let ((predicate (cadr expr))
                 (consequent (caddr expr))
                 (alternate (cadddr expr)))
             `(if ,(loop predicate (empty-context))
                  ,(loop consequent context)
                  ,(loop alternate context))))
          ((let-form? expr)
           (let ((bindings (cadr expr))
                 (body (caddr expr)))
             (if (null? (cdddr expr))
                 `(let ,(map cons (map car bindings)
                             (map (lambda (expr)
                                    (loop expr (empty-context)))
                                  (map cdr bindings)))
                    ,(loop body context))
                 (error "Malformed LET" expr))))
          ((and (not (empty-context? context)) (construction? expr))
           (if (compatible? context expr)
               (loop (access context expr) (pop context))
               (error "Type error detected" expr context)))
          ((accessor? expr)
           (loop (cadr expr) (push-access expr context)))
          (else
           (reconstruct context (map (lambda (arg)
                                       (loop arg (empty-context)))
                                     expr))))))
