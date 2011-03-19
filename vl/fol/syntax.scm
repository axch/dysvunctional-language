(declare (usual-integrations))
;;;; Syntax and manipulations of the output language

(define let-form? (tagged-list? 'let))
(define if-form? (tagged-list? 'if))

(define ->lambda
  (rule `(let (? bindings) (?? body))
        `((lambda ,(map car bindings)
            ,@body)
          ,@(map cadr bindings))))

(define ->let
  (rule `((lambda (? names) (?? body)) (?? args))
        `(let ,(map list names args) ,@body)))

(define reconstitute-definition
  (iterated
   (rule `(define (? name)
            (lambda (? names)
              (?? body)))
         `(define (,name ,@names)
            ,@body))))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
           (memq (car exp) '(cons vector real car cdr vector-ref))
           (every constructors-only? (cdr exp)))))

(define (occurs-in-tree? thing tree)
  (cond ((equal? thing tree) #t)
        ((pair? tree)
         (or (occurs-in-tree? thing (car tree))
             (occurs-in-tree? thing (cdr tree))))
        (else #f)))

(define (filter-map-tree proc tree)
  (let walk ((tree tree) (answer '()))
    (if (pair? tree)
        (walk (car tree) (walk (cdr tree) answer))
        (let ((elt (proc tree)))
          (if elt
              (cons elt answer)
              answer)))))

(define (count-free-occurrences name exp)
  (cond ((eq? exp name) 1)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (lambda-formal exp))
             0
             (count-free-occurrences name (cddr exp))))
        ((let-form? exp)
         (count-free-occurrences name (->lambda exp)))
        ((pair-form? exp)
         (+ (count-free-occurrences name (car-subform exp))
            (count-free-occurrences name (cdr-subform exp))))
        ((pair? exp)
         (+ (count-free-occurrences name (car exp))
            (count-free-occurrences name (cdr exp))))
        (else 0)))

(define (replace-free-occurrences name new exp)
  (cond ((eq? exp name) new)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (lambda-formal exp))
             exp
             `(lambda ,(lambda-formal exp)
                ,@(replace-free-occurrences name new (cddr exp)))))
        ((let-form? exp)
         (->let (replace-free-occurrences name new (->lambda exp))))
        ((pair-form? exp)
         `(cons ,(replace-free-occurrences name new (car-subform exp))
                ,(replace-free-occurrences name new (cdr-subform exp))))
        ((pair? exp)
         (cons (replace-free-occurrences name new (car exp))
               (replace-free-occurrences name new (cdr exp))))
        (else exp)))

(define (alpha-extend env names)
  (append
   (map cons
        names
        (map (lambda (name)
               (if (assq name env)
                   (make-name (symbol name '-))
                   name))
             names))
   env))

(define (alpha-rename exp env)
  (cond ((assq exp env) => cdr)
        ((lambda-form? exp)
         (let* ((names (cadr exp))
                (body (cddr exp))
                (new-env (alpha-extend env names))
                (new-names (map (lambda (name)
                                  (cdr (assq name new-env)))
                                names)))
           `(lambda ,new-names
              ,@(alpha-rename body new-env))))
        ((let-form? exp)
         (->let (alpha-rename (->lambda exp) env)))
        ((definition? exp)
         ;; Assume the definiendum is already unique
         (reconstitute-definition
          `(define ,(definiendum exp)
             ,(alpha-rename (definiens exp) env))))
        ((pair? exp)
         (cons (alpha-rename (car exp) env) (alpha-rename (cdr exp) env)))
        (else exp)))

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

;;;; "Runtime system"

(define (fol-eval code)
  (eval code (nearest-repl/environment)))

(define-syntax argument-types
  (syntax-rules ()
    ((_ arg ...)
     (begin))))
