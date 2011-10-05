(declare (usual-integrations))

(define-integrable (pair thing win lose)
  (if (pair? thing)
      (win (car thing) (cdr thing))
      (lose)))

(define-syntax case*
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((expr (cadr form))
           (expr-name (generate-uninterned-symbol 'expr-)))
       (define (arrow-form? clause)
         (and (= 3 (length clause))
              (compare (rename '=>) (cadr clause))))
       (define (ignore? thing)
         (compare thing (rename '_)))
       (define (parse-clause clause lose-name)
         (define (arrow-clause matcher procedure)
           `(,matcher ,expr-name ,procedure ,lose-name))
         (define (standard-clause expr-name pattern body)
           (cond ((pair? pattern)
                  (receive (variables body)
                    (let loop ((subpatterns (cdr pattern)))
                      (cond ((null? subpatterns) (values '() body))
                            ((pair? (car subpatterns))
                             (let ((variable (generate-uninterned-symbol 'part-)))
                               (receive (variables body) (loop (cdr subpatterns))
                                 (values (cons variable variables)
                                         (list (standard-clause variable (car subpatterns) body))))))
                            ;; Assume identifier
                            ((ignore? (car subpatterns))
                             (let ((variable (generate-uninterned-symbol 'dead-)))
                               (receive (variables body) (loop (cdr subpatterns))
                                 (values (cons variable variables)
                                         (cons `(declare (ignore ,variable))
                                               body)))))
                            (else ;; Assume identifier
                             (receive (variables body) (loop (cdr subpatterns))
                               (values (cons (car subpatterns) variables)
                                       body)))))
                           `(,(car pattern) ,expr-name (,(rename 'lambda) ,variables ,@body) ,lose-name)))
                 ((ignore? pattern)
                  `(let ()
                     (declare (ignore ,lose-name))
                     ,@body))
                 (else
                  `(let ((,pattern ,expr-name))
                     (declare (ignore ,lose-name))
                     ,@body))))
         (if (arrow-form? clause)
             (arrow-clause (car clause) (caddr clause))
             (standard-clause expr-name (car clause) (cdr clause))))
       `(,(rename 'let) ((,expr-name ,expr))
          ,(let loop ((clauses (cddr form)))
             (if (null? clauses)
                 (rename 'unspecific)
                 (let ((lose-name (generate-uninterned-symbol 'lose-)))
                   `(,(rename 'let) ((,lose-name (,(rename 'lambda) () ,(loop (cdr clauses)))))
                     ,(parse-clause (car clauses) lose-name))))))))))

(define (do-it thing)
  (case* thing
         ((pair (pair a d) dd) (+ a d dd))
         ((pair a d) (+ a d))))

(define (my-do-it thing)
  (if (pair? thing)
      (let ((a (car thing)) (d (cdr thing)))
        (if (pair? a)
            (+ (car a) (cdr a) d)
            (+ a d)))))

(define (do-it2 thing)
  (case* thing
   ((pair _ (pair _ d)) d)
   (_ thing)))

(define (my-do-it2 thing)
  (if (pair? thing)
      (let ((d (cdr thing)))
        (if (pair? d)
            (cdr d)
            thing))
      thing))

(define (test-it thing count)
  (show-time
   (lambda ()
     (let loop ((count count))
       (if (= count 0)
           'ok
           (begin
             (do-it2 thing)
             (loop (- count 1))))))))

;; TODO as patterns; underscores?; else clause?
