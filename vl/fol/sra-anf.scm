(declare (usual-integrations))

(define begin-form? (tagged-list? 'begin))

(define (accessor? expr)
  (or (cons-ref? expr)
      (vector-ref? expr)))
(define (cons-ref? expr)
  (and (pair? expr) (pair? (cdr expr)) (null? (cddr expr))
       (memq (car expr) '(car cdr))))
(define (vector-ref? expr)
  (and (pair? expr) (pair? (cdr expr)) (pair? (cddr expr)) (null? (cdddr expr))
       (eq? (car expr) 'vector-ref) (number? (caddr expr))))
(define (construction? expr)
  (and (pair? expr)
       (memq (car expr) '(cons vector))))
(define (push-access expr1 expr2)
  `(,(car expr1) ,expr2 ,@(cddr expr1)))

(define (sra-anf expr)
  (define (rename-nontrivial-expression expr win)
    (cond ((symbol? expr) (win expr '()))
          ((number? expr) (win expr '()))
;;           ((accessor? expr)
;;            (rename-nontrivial-subexpressions
;;             (cadr expr)
;;             (lambda (result names)
;;               (win (push-access expr result) names))))
;;           ((construction? expr)
;;            (rename-nontrivial-expressions
;;             (cdr expr)
;;             (lambda (results names)
;;               (win (cons (car expr) results) names))))
          (else
           (let ((name (make-name 'anf)))
             (win name `((,name ,expr)))))))
  (define (rename-nontrivial-expressions exprs win)
    (if (null? exprs)
        (win '() '())
        (rename-nontrivial-expression
         (car exprs)
         (lambda (result names)
           (rename-nontrivial-expressions (cdr exprs)
            (lambda (results more-names)
              (win (cons result results)
                   (append names more-names))))))))
  (let loop ((expr expr))
    (cond ((symbol? expr) expr)
          ((number? expr) expr)
          ((boolean? expr) expr)
          ((null? expr) expr)
          ((if-form? expr)
           `(if ,(loop (cadr expr))
                ,(loop (caddr expr))
                ,(loop (cadddr expr))))
          ((let-form? expr)
           (if (null? (cdddr expr))
               `(let ,(map (lambda (binding)
                             `(,(car binding) ,(loop (cadr binding))))
                           (cadr expr))
                  ,(loop (caddr expr)))
               (error "Malformed LET" expr)))
          ((begin-form? expr)
           (map loop expr))
          ((definition? expr)
           ((rule `(define (? formals)
                     (argument-types (?? stuff))
                     (? body))
                  `(define ,formals
                     (argument-types ,@stuff)
                     ,(loop body)))
            expr))
          (else ; application
           (rename-nontrivial-expressions
            expr
            (lambda (results names)
              (if (not (null? names))
                  (loop `(let ,names ,results))
                  expr)))))))
