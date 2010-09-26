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

(define *symbol-count* 0)

(define (make-name prefix)
  (set! *symbol-count* (+ *symbol-count* 1))
  (symbol prefix *symbol-count*))

(define (vl-variable->scheme-variable var)
  var)

(define (vl-variable->scheme-field-name var)
  var)

(define (vl-variable->scheme-record-access var closure)
  `(,(symbol (abstract-closure->scheme-structure-name closure)
	     '- (vl-variable->scheme-field-name var))
    the-closure))

(define (fresh-temporary)
  (make-name 'temp-))

(define *closure-names* (make-abstract-hash-table))

(define (abstract-closure->scheme-structure-name closure)
  (hash-table/lookup *closure-names* closure
   (lambda (value)
     value)
   (lambda ()
     (let ((answer (make-name 'closure-)))
       (hash-table/put! *closure-names* closure answer)
       answer))))

(define (abstract-closure->scheme-constructor-name closure)
  (symbol 'make- (abstract-closure->scheme-structure-name closure)))

(define *call-site-names* (make-abstract-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/lookup *call-site-names* (cons closure abstract-arg)
   (lambda (value)
     value)
   (lambda ()
     (let ((answer (make-name 'operation-)))
       (hash-table/put! *call-site-names* (cons closure abstract-arg) answer)
       answer))))

(define (initialize-name-cahces!)
  (set! *symbol-count* 0)
  (set! *closure-names* (make-abstract-hash-table))
  (set! *call-site-names* (make-abstract-hash-table)))

(define (compile exp full-env enclosure analysis)
  (let ((value (refine-eval-once exp full-env analysis)))
    (if (solved-abstractly? value)
	(solved-abstract-value->constant value)
	(cond ((constant? exp) exp)
	      ((null? exp) ''())
	      ((variable? exp)
	       (compile-variable exp enclosure))
	      ((pair? exp)
	       (cond ((eq? (car exp) 'lambda)
		      (compile-lambda exp full-env enclosure analysis))
		     ((eq? (car exp) 'cons)
		      (compile-cons exp full-env enclosure analysis))
		     (else
		      (compile-apply exp full-env enclosure analysis))))
	      (else
	       (error "Invaid expression in code generation"
		      exp full-env enclosure analysis))))))

(define (compile-variable exp enclosure)
  (if (memq exp (closure-free-variables enclosure))
      (vl-variable->scheme-record-access exp enclosure)
      (vl-variable->scheme-variable exp)))

(define (compile-lambda exp full-env enclosure analysis)
  (cons (abstract-closure->scheme-constructor-name
	 (refine-eval-once exp full-env analysis))
	(map (lambda (var)
	       (compile var full-env enclosure analysis))
	     (filter (interesting-variable? full-env)
		     (free-variables exp)))))

(define (compile-cons exp full-env enclosure analysis)
  (let ((first (cadr exp))
	(second (caddr exp)))
    (let ((first-shape (refine-eval-once first full-env analysis))
	  (second-shape (refine-eval-once second full-env analysis)))
      `(cons ,(if (solved-abstractly? first-shape)
		  (solved-abstract-value->constant first-shape)
		  (compile first full-env enclosure analysis))
	     ,(if (solved-abstractly? second-shape)
		  (solved-abstract-value->constant second-shape)
		  (compile second full-env enclosure analysis))))))

(define (compile-apply exp full-env enclosure analysis)
  (let ((operator (refine-eval-once (car exp) full-env analysis))
	(operands (refine-eval-once (cadr exp) full-env analysis)))
    (cond ((eq? primitive-if operator)
	   (generate-if-statement
	    exp full-env enclosure analysis operator operands))
	  ((primitive? operator)
	   (primitive-application
	    operator
	    (compile (cadr exp) full-env enclosure analysis)))
	  ((closure? operator)
	   (closure-application operator operands
	    (lambda ()
	      (compile (car exp) full-env enclosure analysis))
	    (lambda ()
	      (compile (cadr exp) full-env enclosure analysis))))
	  (else
	   (error "Invalid operator in code generation"
		  exp operator operands full-env analysis)))))

(define (generate-if-statement exp full-env enclosure analysis operator operands)
  (define (if-procedure-expression-consequent exp)
    (cadr (caddr (cadr exp))))
  (define (if-procedure-expression-alternate exp)
    (caddr (caddr (cadr exp))))
  (define (generate-if-branch invokee-shape branch-exp)
    (let ((answer-shape (abstract-result-of invokee-shape analysis)))
      (if (solved-abstractly? answer-shape)
	  (solved-abstract-value->constant answer-shape)
	  (closure-application
	   invokee-shape '()
	   (lambda ()
	     (compile branch-exp full-env enclosure analysis))
	   (lambda ()
	     (error "Lose!"))))))
  `(if ,(compile (cadr (cadr exp)) full-env enclosure analysis)
       ,(generate-if-branch
	 (cadr operands) (if-procedure-expression-consequent exp))
       ,(generate-if-branch
	 (cddr operands) (if-procedure-expression-alternate exp))))

(define (primitive-application primitive arg-code)
  (cond ((= 1 (primitive-arity primitive))
	 `(,(primitive-name primitive) ,arg-code))
	((= 2 (primitive-arity primitive))
	 (let ((temp (fresh-temporary)))
	   `(let ((,temp ,arg-code))
	      (,(primitive-name primitive) (car ,temp) (cdr ,temp)))))
	(else
	 (error "Unsupported arity of primitive operation" primitive))))

(define (closure-application closure arg-shape compile-closure compile-arg)
  (let ((call-name (call-site->scheme-function-name closure arg-shape)))
    (if (solved-abstractly? closure)
	(if (solved-abstractly? arg-shape)
	    (list call-name)
	    (list call-name (compile-arg)))
	(if (solved-abstractly? arg-shape)
	    (list call-name (compile-closure))
	    (list call-name (compile-closure) (compile-arg))))))

(define (needs-structure-definition? abstract-value)
  (and (closure? abstract-value)
       (not (solved-abstractly? abstract-value))))

(define (structure-definitions analysis)
  (map abstract-value->structure-definition
       (delete-duplicates
	(filter needs-structure-definition?
		(map caddr (analysis-bindings analysis)))
	abstract-equal?)))

(define (abstract-value->structure-definition value)
  (cond ((closure? value)
	 `(define-structure ,(abstract-closure->scheme-structure-name value)
	    ,@(map vl-variable->scheme-field-name
		   (filter (interesting-variable? (closure-env value))
			   (closure-free-variables value)))))
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
		       (extend-abstract-env
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
	   (let ((operator (refine-eval-once (car exp) env analysis))
		 (operands (refine-eval-once (cadr exp) env analysis)))
	     (and (closure? operator)
		  (cons operator operands))))))
  (map procedure-definition
       (delete-duplicates
	(filter (lambda (x) x)
		(map maybe-pocedure-request (analysis-bindings analysis)))
	abstract-equal?)))

(define (compile-to-scheme program #!optional print-analysis?)
  (initialize-name-cahces!)
  (let ((analysis (analyze program)))
    (if (and (not (default-object? print-analysis?))
	     print-analysis?)
	(pp analysis))
    (let ((answer
	   `(begin ,@(structure-definitions analysis)
		   ,@(procedure-definitions analysis)
		   ,(compile (macroexpand program)
			     (env->abstract-env (initial-vl-user-env))
			     #f
			     analysis))))
      (if peephole-optimize
	  (peephole-optimize answer)
	  answer))))
