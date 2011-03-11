(declare (usual-integrations))
;;;; FOL (First-Order Language)

;;; FOL is a simple-minded first-order language, which serves as the
;;; compilation target for VL and DVL.  FOL has several design
;;; objectives:
;;;   FOL must be a convenient target for code generation after flow analysis
;;;   FOL must be compilable to efficient machine code
;;;   FOL should not be unduly illegible
;;; In pursuit of these design objectives, FOL is a first-oder subset
;;; of MIT Scheme, supporting a limited range of constructs.
;;;
;;; The syntax of FOL is expressed as Scheme data structures.  This
;;; may be parsed from a file or constructed directly in memory, as
;;; appropriate for the application.
;;;
;;; FOL follows the following grammar:
;;;
;;; program    = (begin <definition> ... <expression>)
;;;            | <expression>
;;;
;;; definition = (define (<proc-var> <data-var> ...) <expression>)
;;;
;;; expression = <data-var>
;;;            | <number>
;;;            | (<proc-var> <expression> ...)
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)
;;;
;;; FOL distinguishes two types of variables, one for holding
;;; procedures and one for holding data.  Both are Scheme symbols; the
;;; procedure variables have global scope, must be globally unique,
;;; and may only be bound by DEFINE forms.
;;;

;;; Procedures may only be defined at the top level.  Procedure
;;; names must be unique.

;;; A FOL program is a list, whose first symbol is BEGIN, whose last
;;; element is a FOL expression, and all of whose intermediate
;;; elements are FOL procedure definitions.  In the event of absence
;;; of procedure definitions, the outer list with the BEGIN may be
;;; omitted.

;;; has the following special forms:
;;; begin, define, if, let

;;;; Syntax and manipulations of the output language

(define let-form? (tagged-list? 'let))

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

(define (count-in-tree thing tree)
  (cond ((equal? thing tree) 1)
	((pair? tree)
	 (+ (count-in-tree thing (car tree))
	    (count-in-tree thing (cdr tree))))
	(else 0)))

(define (occurs-in-tree? thing tree)
  (> (count-in-tree thing tree) 0))

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
	 (let ((names (lambda-formal exp))
	       (body (lambda-body exp)))
	   (let ((new-env (alpha-extend env names)))
	     (make-lambda-form
	      (map (lambda (name)
		     (cdr (assq name new-env)))
		   names)
	      (alpha-rename body new-env)))))
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
