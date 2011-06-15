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

;; TODO Would rewriting this as a pass accelerate things
;; significantly?
;; TODO Can/Should I lift lets out of IF predicates?  Procedure
;; calls?  CAR/CDR/CONS?
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

;;; Flushing type signatures

;; Flushing type signatures is only useful for viewing a program, as
;; the result is not valid FOL any more.

(define remove-defn-argument-types
  (rule `(define (? formals)
           (argument-types (?? etc))
           (?? body))
        `(define ,formals
           ,@body)))

(define strip-argument-types
  (rule-simplifier (list remove-defn-argument-types)))

;;; Reserved words

(define fol-reserved '(cons car cdr vector vector-ref begin define if let let-values values))

;;;; "Runtime system"

(define (fol-eval code)
  (eval (prepare-for-scheme code) fol-environment))
