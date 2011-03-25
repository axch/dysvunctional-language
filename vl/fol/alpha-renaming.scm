(declare (usual-integrations))
;;;; Alpha renaming

(define (extend-alpha-env env names)
  (append
   (map cons
        names
        (map (lambda (name)
               (if (assq name env)
                   (make-name (symbol name '-))
                   name))
             names))
   env))

(define (alpha-rename-exp exp env)
  (cond ((assq exp env) => cdr)
        ((lambda-form? exp)
         (let* ((names (cadr exp))
                (body (cddr exp))
                (new-env (extend-alpha-env env names))
                (new-names (map (lambda (name)
                                  (cdr (assq name new-env)))
                                names)))
           `(lambda ,new-names
              ,@(alpha-rename-exp body new-env))))
        ((let-form? exp)
         (->let (alpha-rename-exp (->lambda exp) env)))
        ((definition? exp)
         ;; Assume the definiendum is already unique
         (reconstitute-definition
          `(define ,(definiendum exp)
             ,(alpha-rename-exp (definiens exp) env))))
        ((pair? exp)
         (cons (alpha-rename-exp (car exp) env)
               (alpha-rename-exp (cdr exp) env)))
        (else exp)))

(define (alpha-rename program)
  ;; TODO Fix the bookkeeping of what names the primitives rely on
  (define (needed-names primitive)
    (list (primitive-name primitive)))
  (alpha-rename-exp program
   (map (lambda (name)
          (cons name name))
        (delete-duplicates
         `(cons car cdr if define let vector vector-ref
                ,@(append-map needed-names *primitives*))))))

(define (alpha-rename? exp1 exp2)
  (let loop ((exp1 exp1)
             (exp2 exp2)
             (env '()))
    (cond ((assq exp1 env) => (lambda (bind) (equal? (cdr bind) exp2)))
          ((and (lambda-form? exp1) (lambda-form? exp2))
           (let* ((names1 (cadr exp1))
                  (body1 (cddr exp1))
                  (names2 (cadr exp2))
                  (body2 (cddr exp2))
                  (new-env (append (map cons names1 names2) env)))
             (loop body1 body2 new-env)))
          ((and (let-form? exp1) (let-form? exp2))
           (loop (->lambda exp1) (->lambda exp2) env))
          ((and (definition? exp1) (definition? exp2))
           (let ((name1 (definiendum exp1))
                 (name2 (definiendum exp2)))
             (loop (definiens exp1) (definiens exp2) (cons (cons name1 name2) env))))
          ((and (pair? exp1) (pair? exp2))
           (and (loop (car exp1) (car exp2) env)
                (loop (cdr exp1) (cdr exp2) env)))
          (else (equal? exp1 exp2)))))
