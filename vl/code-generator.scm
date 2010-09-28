;;;; Code generator

;;; The computed analysis provides a complete description of what's
;;; going on in a program.  The code generator produces code in the
;;; target language (in this case, MIT Scheme) that's structurally
;;; isomorphic to the original, specialized VL code.  The idea is to
;;; create an MIT Scheme procedure definition for every pair of VL
;;; closure and abstract value it gets applied to (if the result is a
;;; non-void abstract value); an MIT Scheme structure definition for
;;; every compound structure (including converted closures) that
;;; occurs; and an MIT Scheme expression for the entry point.  This
;;; pile should have the property that all function calls are to known
;;; targets.

(define (compile exp env enclosure analysis)
  (let ((value (analysis-get exp env analysis)))
    (if (solved-abstractly? value)
	(solved-abstract-value->constant value)
	(cond ((constant? exp) exp)
	      ((null? exp) ''())
	      ((variable? exp)
	       (compile-variable exp enclosure))
	      ((pair? exp)
	       (cond ((eq? (car exp) 'lambda)
		      (compile-lambda exp env enclosure analysis))
		     ((eq? (car exp) 'cons)
		      (compile-cons exp env enclosure analysis))
		     (else
		      (compile-apply exp env enclosure analysis))))
	      (else
	       (error "Invaid expression in code generation"
		      exp env enclosure analysis))))))

(define (compile-variable exp enclosure)
  (if (memq exp (closure-free-variables enclosure))
      (vl-variable->scheme-record-access exp enclosure)
      (vl-variable->scheme-variable exp)))

(define (compile-lambda exp env enclosure analysis)
  (cons (abstract-closure->scheme-constructor-name
	 (analysis-get exp env analysis))
	(map (lambda (var)
	       (compile var env enclosure analysis))
	     (sort (filter (interesting-variable? env)
			   (free-variables exp))
		   symbol<?))))

(define (compile-cons exp env enclosure analysis)
  (let ((first (cadr exp))
	(second (caddr exp)))
    (let ((first-shape (analysis-get first env analysis))
	  (second-shape (analysis-get second env analysis)))
      `(cons ,(if (solved-abstractly? first-shape)
		  (solved-abstract-value->constant first-shape)
		  (compile first env enclosure analysis))
	     ,(if (solved-abstractly? second-shape)
		  (solved-abstract-value->constant second-shape)
		  (compile second env enclosure analysis))))))

(define (compile-apply exp env enclosure analysis)
  (let ((operator (analysis-get (car exp) env analysis))
	(operands (analysis-get (cadr exp) env analysis)))
    (cond ((eq? primitive-if operator)
	   (generate-if-statement
	    exp env enclosure analysis operands))
	  ((primitive? operator)
	   (generate-primitive-application
	    operator
	    (compile (cadr exp) env enclosure analysis)))
	  ((closure? operator)
	   (generate-closure-application
	    operator operands
	    (compile (car exp) env enclosure analysis)
	    (compile (cadr exp) env enclosure analysis)))
	  (else
	   (error "Invalid operator in code generation"
		  exp operator operands env analysis)))))

(define (generate-if-statement exp env enclosure analysis operands)
  (define (if-procedure-expression-consequent exp)
    (cadr (caddr (cadr exp))))
  (define (if-procedure-expression-alternate exp)
    (caddr (caddr (cadr exp))))
  (define (generate-if-branch invokee-shape branch-exp)
    (let ((answer-shape (abstract-result-of invokee-shape analysis)))
      (if (solved-abstractly? answer-shape)
	  (solved-abstract-value->constant answer-shape)
	  (generate-closure-application
	   invokee-shape '()
	   (compile branch-exp env enclosure analysis)
	   "Lose!"))))
  (if (solved-abstractly? (car operands))
      (if (car operands)
	  (generate-if-branch
	   (cadr operands) (if-procedure-expression-consequent exp))
	  (generate-if-branch
	   (cddr operands) (if-procedure-expression-alternate exp)))
      `(if ,(compile (cadr (cadr exp)) env enclosure analysis)
	   ,(generate-if-branch
	     (cadr operands) (if-procedure-expression-consequent exp))
	   ,(generate-if-branch
	     (cddr operands) (if-procedure-expression-alternate exp)))))

(define (generate-primitive-application primitive arg-code)
  (cond ((= 1 (primitive-arity primitive))
	 `(,(primitive-name primitive) ,arg-code))
	((= 2 (primitive-arity primitive))
	 (let ((temp (fresh-temporary)))
	   `(let ((,temp ,arg-code))
	      (,(primitive-name primitive) (car ,temp) (cdr ,temp)))))
	(else
	 (error "Unsupported arity of primitive operation" primitive))))

(define (generate-closure-application closure arg-shape closure-code arg-code)
  (let ((call-name (call-site->scheme-function-name closure arg-shape)))
    (if (solved-abstractly? closure)
	(if (solved-abstractly? arg-shape)
	    (list call-name)
	    (list call-name arg-code))
	(if (solved-abstractly? arg-shape)
	    (list call-name closure-code)
	    (list call-name closure-code arg-code)))))

(define (structure-definitions analysis)
  (map abstract-value->structure-definition
       (delete-duplicates
	(filter needs-structure-definition?
		(map caddr (analysis-bindings analysis)))
	abstract-equal?)))

(define (needs-structure-definition? abstract-value)
  (and (closure? abstract-value)
       (not (solved-abstractly? abstract-value))))

(define (abstract-value->structure-definition value)
  (cond ((closure? value)
	 `(define-structure ,(abstract-closure->scheme-structure-name value)
	    ,@(map vl-variable->scheme-field-name
		   (sort (filter (interesting-variable? (closure-env value))
				 (closure-free-variables value))
			 symbol<?))))
	(else (error "Not compiling non-closure aggregates to Scheme structures"
		     value))))

(define (procedure-definitions analysis)
  (define (destructuring-let-bindings formal-tree arg-tree)
    (define (xxx part1 part2)
      (append (replace-in-tree
	       'the-formals '(car the-formals)
	       (destructuring-let-bindings part1 (car arg-tree)))
	      (replace-in-tree
	       'the-formals '(cdr the-formals)
	       (destructuring-let-bindings part2 (cdr arg-tree)))))
    (cond ((null? formal-tree)
	   '())
	  ((symbol? formal-tree)
	   (if (solved-abstractly? arg-tree)
	       '()
	       `((,formal-tree the-formals))))
	  ((pair? formal-tree)
	   (if (eq? (car formal-tree) 'cons)
	       (xxx (cadr formal-tree) (caddr formal-tree))
	       (xxx (car formal-tree) (cdr formal-tree))))))
  (define (procedure-definition pair)
    (let ((operator (car pair))
	  (operands (cdr pair)))
      (let ((name (call-site->scheme-function-name operator operands)))
	`(define (,name ,@(if (solved-abstractly? operator)
			      '()
			      '(the-closure))
			,@(if (solved-abstractly? operands)
			      '()
			      '(the-formals)))
	   (let ,(destructuring-let-bindings
		  (car (closure-formal operator))
		  operands)
	     ,(compile (closure-body operator)
		       (extend-env
			(closure-formal operator)
			operands
			(closure-env operator))
		       operator				   
		       analysis))))))
  (define (maybe-pocedure-request binding)
    (let ((exp (car binding))
	  (env (cadr binding))
	  (value (caddr binding)))
      (and (not (solved-abstractly? value))
	   (pair? exp)
	   (not (eq? (car exp) 'cons))
	   (not (eq? (car exp) 'lambda))
	   (let ((operator (analysis-get (car exp) env analysis))
		 (operands (analysis-get (cadr exp) env analysis)))
	     (and (closure? operator)
		  (cons operator operands))))))
  (map procedure-definition
       (delete-duplicates
	(filter (lambda (x) x)
		(map maybe-pocedure-request (analysis-bindings analysis)))
	abstract-equal?)))

(define (compile-to-scheme program #!optional print-analysis?)
  (initialize-name-caches!)
  (let ((analysis (analyze program)))
    (if (and (not (default-object? print-analysis?))
	     print-analysis?)
	(pp analysis))
    (let ((answer
	   `(begin ,@(structure-definitions analysis)
		   ,@(procedure-definitions analysis)
		   ,(compile (macroexpand program)
			     (initial-vl-user-env)
			     #f
			     analysis))))
      (if peephole-optimize
	  (peephole-optimize answer)
	  answer))))
