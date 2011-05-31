(declare (usual-integrations))
;;;; Syntax and manipulations of the output language

(define ((tagged-list? tag) thing)
  (and (pair? thing)
       (eq? (car thing) tag)))

(define begin-form? (tagged-list? 'begin))

(define (tidy-begin form)
  (if (and (begin-form? form) (null? (cddr form)))
      (cadr form)
      form))

(define definition? (tagged-list? 'define))

(define (normalize-definition definition)
  (cond ((not (definition? definition))
         (error "Trying to normalize a non-definition" definition))
        ((pair? (cadr definition))
         (normalize-definition
          `(define ,(caadr definition)
             (lambda ,(cdadr definition)
               ,@(cddr definition)))))
        (else definition)))

(define (definiendum definition)
  (cadr (normalize-definition definition)))

(define (definiens definition)
  (caddr (normalize-definition definition)))

(define reconstitute-definition
  (iterated
   (rule `(define (? name)
            (lambda (? names)
              (?? body)))
         `(define (,name ,@names)
            ,@body))))

(define (fol-var? thing)
  (or (symbol? thing)
      (fol-name? thing)))

(define (fol-const? thing)
  (or (number? thing) (boolean? thing) (null? thing)))

(define (simple-form? thing)
  (or (fol-var? thing) (number? thing) (boolean? thing) (null? thing)))

(define if-form? (tagged-list? 'if))

(define let-form? (tagged-list? 'let))
(define let-values-form? (tagged-list? 'let-values))
(define lambda-form? (tagged-list? 'lambda))

(define (binder-tag? thing)
  (or (eq? thing 'let)
      (eq? thing 'let-values)))

(define ->lambda
  (rule-list
   (list
    (rule `(let (? bindings) (?? body))
          `((lambda ,(map car bindings)
              ,@body)
            ,@(map cadr bindings)))
    ;; This does not produce a syntactically valid object, but it does
    ;; accurately capture what the names are doing.
    (rule `(let-values (((? names) (? exp))) (?? body))
          `((lambda ,names ,@body)
            ,exp)))))

(define ->let
  (rule `((lambda (? names) (?? body)) (?? args))
        `(let ,(map list names args) ,@body)))

(define ->let-values
  (rule `((lambda (? names) (?? body)) (? exp))
        `(let-values ((,names ,exp))
           ,@body)))

(define (accessor? expr)
  (or (cons-ref? expr)
      (vector-ref? expr)))

(define (cons-ref? expr)
  (and (pair? expr)
       (memq (car expr) '(car cdr))))

(define vector-ref? (tagged-list? 'vector-ref))

(define (construction? expr)
  (and (pair? expr)
       (memq (car expr) '(cons vector))))

(define values-form? (tagged-list? 'values))

(define remove-defn-argument-types
  (rule `(define (? formals)
           (argument-types (?? etc))
           (?? body))
        `(define ,formals
           ,@body)))

(define strip-argument-types
  (rule-simplifier (list remove-defn-argument-types)))

(define fol-reserved '(cons car cdr vector vector-ref begin define if let let-values values))

(define (count-free-occurrences name exp)
  (cond ((eq? exp name) 1)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (cadr exp))
             0
             (count-free-occurrences name (cddr exp))))
        ((or (let-values-form? exp) (let-form? exp))
         (count-free-occurrences name (->lambda exp)))
        ((pair? exp)
         (+ (count-free-occurrences name (car exp))
            (count-free-occurrences name (cdr exp))))
        (else 0)))

(define (replace-free-occurrences name new exp)
  (cond ((eq? exp name) new)
        ((lambda-form? exp)
         (if (occurs-in-tree? name (cadr exp))
             exp
             `(lambda ,(cadr exp)
                ,@(replace-free-occurrences name new (cddr exp)))))
        ((let-form? exp)
         (->let (replace-free-occurrences name new (->lambda exp))))
        ((let-values-form? exp)
         (->let-values (replace-free-occurrences name new (->lambda exp))))
        ((pair? exp)
         (cons (replace-free-occurrences name new (car exp))
               (replace-free-occurrences name new (cdr exp))))
        (else exp)))

;;;; "Runtime system"

(define (fol-eval code)
  (eval (prepare-for-scheme code) fol-environment))
