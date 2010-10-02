(declare (usual-integrations))
;;;; Code generator

;;; The computed analysis provides a complete description of what's
;;; going on in a program, except for which specific real numbers
;;; happen to be where.  The code generator's job is to produce
;;; efficient code for computing that residual.

;;; The code generator produces code in the target language (in this
;;; case, Scheme) that's structurally isomorphic to the original,
;;; specialized VL code.  There is a Scheme structure definition for
;;; every VL closure that closes over any non-solved VL value (VL
;;; pairs become Scheme pairs).  There is a Scheme procedure
;;; definition for every pair of VL closure and abstract value it gets
;;; applied to (if the result is a non-solved abstract value).  The
;;; body of this procedure will have IF statements and procedure calls
;;; for every non-solved VL IF statement and procedure call, but the
;;; targets of those procedure calls can all be direct names naming
;;; procedures that Scheme will know statically.  Also all those
;;; procedures are closure-converted and defined at the top level,
;;; thus not using the fact that Scheme is itself higher-order.
;;; Finally, there is a Scheme expression for the entry point.

;;; This code generator follows the notes and equations in Section 6
;;; of [1], with the obvious difference that it targets Scheme instead
;;; of C.  Also primitives are spelled out, and IF statements again
;;; prove surprisingly complicated.  I also found that the COMPILE
;;; recursion needed to carry around the analyzed closure object
;;; representing the VL procedure whose body is being compiled,
;;; because the Scheme code generated for fetching a free VL variable
;;; from its converted closure record needs to know the name of the
;;; record type, whereas for C one could just write the ubiquitous
;;; ".".

;;; [1] Jeffrey Siskind and Barak Pearlmutter, "Using Polyvariant
;;; Union-Free Flow Analysis to Compile a Higher-Order Functional
;;; Programming Language with a First-Class Derivative Operator to
;;; Efficient Fortran-like Code."  Purdue University ECE Technical
;;; Report, 2008.  http://docs.lib.purdue.edu/ecetr/367
;;;; Compiling expressions

;;; Compilation of expressions proceeds by structural recursion on the
;;; expression, paying attention to portions whose values are
;;; completely solved by the analysis because those need not be
;;; computed (and their values can just be put where they are needed).

;;; COMPILE is C from [1].
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
	      (else (error "Invaid expression in code generation"
			   exp env enclosure analysis))))))

;;; A VL variable access becomes either a Scheme variable access if
;;; the VL variable was bound by the immediately nearest VL LAMBDA, or
;;; a Scheme record access to the converted closure if it was free.
(define (compile-variable exp enclosure)
  (if (memq exp (closure-free-variables enclosure))
      (vl-variable->scheme-record-access exp enclosure)
      (vl-variable->scheme-variable exp)))

;;; Closure conversion: A VL LAMBDA form becomes the construction of a
;;; Scheme record.  The record has slots only for values that were not
;;; solved by the flow analysis, ordered by their VL variable names.
(define (compile-lambda exp env enclosure analysis)
  (cons (abstract-closure->scheme-constructor-name
	 (analysis-get exp env analysis))
	(map (lambda (var) (compile var env enclosure analysis))
	     (interesting-variables exp env))))

;;; A VL CONS becomes a Scheme CONS.
(define (compile-cons exp env enclosure analysis)
  (let ((first-shape (analysis-get (cadr exp) env analysis))
	(second-shape (analysis-get (caddr exp) env analysis)))
    `(cons ,(if (solved-abstractly? first-shape)
		(solved-abstract-value->constant first-shape)
		(compile (cadr exp) env enclosure analysis))
	   ,(if (solved-abstractly? second-shape)
		(solved-abstract-value->constant second-shape)
		(compile (caddr exp) env enclosure analysis)))))
;;; The flow analysis fully determines the shape of every VL procedure
;;; that is called at any VL call site.  This allows applications to
;;; be coded to directly refer to the right target.  N.B.  The
;;; particular way I handled VL's IF makes it register as a primitive
;;; procedure, which needs to be handled specially.
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

;;; A VL IF statement becomes a Scheme IF statement (unless the
;;; predicate was solved by the analysis, in which case we can just
;;; use the right branch).
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

;;; A VL primitive application becomes an inlined call to a Scheme
;;; primitive (destructuring the incoming argument if needed).
(define (generate-primitive-application primitive arg-code)
  (cond ((= 1 (primitive-arity primitive))
	 `(,(primitive-name primitive) ,arg-code))
	((= 2 (primitive-arity primitive))
	 (let ((temp (fresh-temporary)))
	   `(let ((,temp ,arg-code))
	      (,(primitive-name primitive) (car ,temp) (cdr ,temp)))))
	(else
	 (error "Unsupported arity of primitive operation" primitive))))

;;; A VL compound procedure application becomes a call to the
;;; generated Scheme procedure that corresponds to the application of
;;; this closure shape to this argument shape.  The union-free-ness
;;; and polyvariance of the analysis ensures that these are all
;;; distinct and can be given distinct toplevel Scheme names.  The
;;; generated Scheme procedures in question accept two arguments: the
;;; closure-record for the compound procedure being called, and the
;;; pair tree of arguments.  One or both may be elided if they were
;;; solved by the analysis.
(define (generate-closure-application
	 closure arg-shape closure-code arg-code)
  (let ((call-name (call-site->scheme-function-name closure arg-shape)))
    (if (solved-abstractly? closure)
	(if (solved-abstractly? arg-shape)
	    (list call-name)
	    (list call-name arg-code))
	(if (solved-abstractly? arg-shape)
	    (list call-name closure-code)
	    (list call-name closure-code arg-code)))))

;;;; Structure definitions

(define (structure-definitions analysis)
  (map abstract-value->structure-definition
       (delete-duplicates
	(filter needs-structure-definition?
		(map caddr (analysis-bindings analysis)))
	abstract-equal?)))

(define (needs-structure-definition? abstract-value)
  (and (closure? abstract-value)
       (not (solved-abstractly? abstract-value))))

;;; Every VL closure that closes over any unsolved values gets closure
;;; converted to a Scheme record that has slots for those unsolved
;;; values.  The slots are ordered by their VL variable names.
(define (abstract-value->structure-definition value)
  (cond ((closure? value)
	 `(define-structure ,(abstract-closure->scheme-structure-name value)
	    ,@(map vl-variable->scheme-field-name
		   (interesting-variables
		    (closure-free-variables value) (closure-env value)))))
	(else
	 (error "Not compiling non-closure aggregates to Scheme structures"
		value))))

;;;; Procedure definitions

(define (procedure-definitions analysis)
  (map (procedure-definition analysis)
       (delete-duplicates
	(filter-map (binding->maybe-call-shape analysis)
		    (analysis-bindings analysis))
	abstract-equal?)))

;;; Every VL application of every compound VL procedure to every
;;; argument shape to which it is ever applied (producing a non-solved
;;; value) needs to become a Scheme procedure definition.
(define ((binding->maybe-call-shape analysis) binding)
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

;;; Every generated Scheme procedure receives two arguments (either or
;;; both of which will be elided if they are completely solved): the
;;; record for the closure this procedure was converted from and the
;;; data structure containing the arguments.  The procedure must
;;; destructure the argument structure the same way the corresponding
;;; VL procedure did, and execute its compiled body.  The
;;; destructuring elides solved slots of the incoming argument
;;; structure.
(define ((procedure-definition analysis) operator.operands)
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
  (let ((operator (car operator.operands))
	(operands (cdr operator.operands)))
    (let ((name (call-site->scheme-function-name operator operands)))
      `(define (,name
		,@(if (solved-abstractly? operator) '() '(the-closure))
		,@(if (solved-abstractly? operands) '() '(the-formals)))
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


;;;; Code generation

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
      (if post-process
	  (post-process answer)
	  answer))))
