(declare (usual-integrations))
;;;; FOL Syntax

(define ((tagged-list? tag) thing)
  (and (pair? thing)
       (eq? (car thing) tag)))

;;; Begin

(define begin-form? (tagged-list? 'begin))

(define (tidy-begin form)
  (if (and (begin-form? form) (null? (cddr form)))
      (cadr form)
      form))

;;; Define

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

;;; Simple forms

(define (fol-var? thing)
  (or (symbol? thing)
      (fol-name? thing)))

(define (fol-const? thing)
  (or (number? thing) (boolean? thing) (null? thing)))

(define (simple-form? thing)
  (or (fol-var? thing) (number? thing) (boolean? thing) (null? thing)))

;;; If

(define if-form? (tagged-list? 'if))

;;; Binders

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

(define tidy-empty-let
  (rule `(let () (? body)) body))

;; Lifting lets is safe assuming the program has unique bound names; if not,
;; can break because of
#;
 (let ((x 3))
   (let ((y (let ((x 4)) x)))
     x))
;; Unfortunately, mere lack of shadowing is not enough, as this can
;; introduce shadowing because of
#;
 (let ((x (let ((y 3)) y)))
   (let ((y 4))
     y))

(define lift-lets
  (rule-simplifier
   (list
    (rule `(let ((?? bindings1)
                 ((? name ,fol-var?) ((? bind ,binder-tag?) (? inner-bindings) (? exp)))
                 (?? bindings2))
             (?? body))
          `(,bind ,inner-bindings
             (let (,@bindings1
                   (,name ,exp)
                   ,@bindings2)
               ,@body)))

    (rule `(let-values (((? names) ((? bind ,binder-tag?) (? inner-bindings) (? exp))))
             (?? body))
          `(,bind ,inner-bindings
             (let-values ((,names ,exp))
               ,@body))))))

(define (%lift-lets program)
  (if (begin-form? program)
      `(begin
         ,@(map lift-lets-definition (except-last-pair (cdr program)))
         ,(lift-lets-expression (last program)))
      (lift-lets-expression program)))

(define lift-lets-definition
  (rule `(define (? formals)
           (argument-types (?? stuff))
           (? body))
        `(define ,formals
           (argument-types ,@stuff)
           ,(lift-lets-expression body))))

(define (lift-lets-expression expr)
  ;; This is written in continuation passing style because the
  ;; recursive call returns two things: the rewritten expression, and
  ;; the list of bindings that this expression seeks to introduce.
  ;; The bindings lists are represented as functions that will wrap a
  ;; given expression in that binding list, for fast appending.
  (define null (lambda (expr) expr))
  (define (singleton var exp)
    (lambda (expr)
      `(let ((,var ,exp))
         ,expr)))
  (define (values-singleton names exp)
    (lambda (expr)
      `(let-values ((,names ,exp))
         ,expr)))
  (define (append lst1 lst2)
    (lambda (expr)
      (lst1 (lst2 expr))))
  (define (build expr lst)
    (lst expr))
  (define (loop expr win)
    (cond ((or (fol-var? expr)
               (fol-const? expr))
           (win expr null))
          ((if-form? expr)
           (lift-lets-from-if expr win))
          ((let-form? expr)
           (lift-lets-from-let expr win))
          ((let-values-form? expr)
           (lift-lets-from-let-values expr win))
          ((lambda-form? expr)
           (lift-lets-from-lambda expr win))
          (else ;; general application
           (lift-lets-from-application expr win))))
  (define (lift-lets-from-if expr win)
    (let ((predicate (cadr expr))
          (consequent (caddr expr))
          (alternate (cadddr expr)))
      (loop predicate
            (lambda (new-pred pred-binds)
              (win `(if ,new-pred
                        ,(lift-lets-expression consequent)
                        ,(lift-lets-expression alternate))
                   pred-binds)))))
  (define (lift-lets-from-let expr win)
    (let ((body (caddr expr)))
      (let per-binding ((bindings (cadr expr))
                        (done null))
        (if (null? bindings)
            (loop body (lambda (new-body body-binds)
                         (win new-body (append done body-binds))))
            (let ((binding (car bindings)))
              (loop (cadr binding)
                    (lambda (new-exp exp-binds)
                      (per-binding
                       (cdr bindings)
                       (append done
                               (append exp-binds
                                       (singleton
                                        (car binding) new-exp)))))))))))
  (define (lift-lets-from-let-values expr win)
    ;; TODO Abstract the commonalities with LET forms?
    (let ((binding (caadr expr))
          (body (caddr expr)))
      (let ((names (car binding))
            (sub-exp (cadr binding)))
        (loop sub-exp
         (lambda (new-sub-expr sub-exp-binds)
           (loop body
            (lambda (new-body body-binds)
              (win new-body
                   (append sub-exp-binds
                           (append (values-singleton names new-sub-expr)
                                   body-binds))))))))))
  (define (lift-lets-from-lambda expr win)
    (win `(lambda ,(cadr expr)
            ,(lift-lets-expression (caddr expr)))
         null))
  (define (lift-lets-from-application expr win)
    ;; In ANF, anything that looks like an application can't have
    ;; nested LETs.
    (win expr null))
  (loop expr build))

;; Converting LET to LET* is only useful for viewing a program, as the
;; result is not valid FOL any more.
(define let->let*
  (rule-simplifier
   (list
    (rule `(let ((? binding))
             (let ((? binding2))
               (?? body)))
          `(let* (,binding
                  ,binding2)
             ,@body))
    (rule `(let ((? binding))
             (let* ((?? bindings))
               (?? body)))
          `(let* (,binding
                  ,@bindings)
             ,@body)))))

;;; Structures

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

(define (tidy-values exp)
  (if (and (values-form? exp)
           (= 2 (length exp)))
      (cadr exp)
      exp))

;;; Measurements
#;
(define (print-fol-statistics program)
  (define defn-statistics
    (rule `(define ((? name) (?? formals))
             (argument-types (?? etc))
             (? body))
          (make-defn-statistics
           1 (length formals))))
  (let ((size (count-pairs program))
        (stripped-size (count-pairs (strip-argument-types program)))
        (defn-count (if (begin-form? program)
                        (length (filter definition? program))
                        0)))))

;;; Flushing type signatures

;; Flushing type signatures is only useful for viewing a program, as
;; the result is not valid FOL any more.

(define remove-defn-argument-types
  (rule `(define (? formals)
           (argument-types (?? etc))
           (?? body))
        `(define ,formals
           ,@body)))

(define (strip-argument-types program)
  (if (begin-form? program)
      (map remove-defn-argument-types program)
      program))

;;; Reserved words

(define fol-reserved '(cons car cdr vector vector-ref begin define if let let-values values))

;;;; "Runtime system"

(define (fol-eval code)
  (eval (prepare-for-scheme code) fol-environment))
