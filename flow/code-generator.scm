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

(define (vl-variable->scheme-record-access var)
  `(record-get the-closure ',(vl-variable->scheme-field-name var)))

(define (fresh-temporary)
  (make-name 'temp-))

;; TODO This is really part of the "runtime system".
;; TODO I can get rid of the awful record access mechanism for getting
;; at closure-converted variables by passing an extra bit of data
;; through the code generator, which is some ID for the type of the
;; closure record that obtains in this place.
(define (record-get record field-name)
  ((record-accessor (record-type-descriptor record)
		    field-name)
   record))

;; TODO Should this really be an eq? hash table, or should I make an
;; abstract-equal? hash table for these?
(define *closure-names* (make-eq-hash-table))

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

;; TODO Can I really get away with an equal? hash table here?
(define *call-site-names* (make-equal-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/lookup *call-site-names* (cons closure abstract-arg)
   (lambda (value)
     value)
   (lambda ()
     (let ((answer (make-name 'operation-)))
       (hash-table/put! *call-site-names* (cons closure abstract-arg) answer)
       answer))))

(define (compile exp full-env free-list analysis)
  (cond ((constant? exp) exp)
	((null? exp) ''())
	((variable? exp)
	 (if (memq exp free-list)
	     (vl-variable->scheme-record-access exp)
	     (vl-variable->scheme-variable exp)))
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		;; TODO I can eliminate void formals between here
		;; and making structure definitions for closures
		(cons (abstract-closure->scheme-constructor-name
		       (refine-eval-once exp full-env analysis))
		      (map (lambda (var)
			     (compile var full-env free-list analysis))
			   (free-variables exp))))
	       ((eq? (car exp) 'cons)
		`(cons ,(compile (cadr exp) full-env free-list analysis)
		       ,(compile (caddr exp) full-env free-list analysis)))
	       (else
		(compile-apply exp full-env free-list analysis))))
	(else
	 (error "Invaid expression in code generation"
		exp full-env free-list analysis))))

(define (compile-apply exp full-env free-list analysis)
  (let ((operator (refine-eval-once (car exp) full-env analysis))
	(operands (refine-eval-once (cadr exp) full-env analysis)))
    (define (primitive-application body)
      (if (primitive-unary? operator)
	  `(,(primitive-name operator) ,body)
	  (let ((temp (fresh-temporary)))
	    `(let ((,temp ,body))
	       (,(primitive-name operator) (car ,temp) (cdr ,temp))))))
    (cond ((primitive? operator)
	   (primitive-application
	    (compile (cadr exp) full-env free-list analysis)))
	  ((closure? operator)
	   `(,(call-site->scheme-function-name operator operands)
	     ,(compile (car exp) full-env free-list analysis)
	     ,(compile (cadr exp) full-env free-list analysis)))
	  (else
	   (error "Invalid operator in code generation"
		  exp full-env analysis)))))

(define (needs-structure-definition? abstract-value)
  (closure? abstract-value))

(define (structure-definitions analysis)
  (map abstract-value->structure-definition
       (filter needs-structure-definition?
	       (map caddr (analysis-bindings analysis)))))

(define (abstract-value->structure-definition value)
  (cond ((closure? value)
	 `(define-structure ,(abstract-closure->scheme-structure-name value)
	    ,@(map vl-variable->scheme-field-name
		   ;; TODO I can eliminate void formals between here
		   ;; and compiling lambda expressions
		   (free-variables `(lambda ,(closure-formal value)
				      ,(closure-body value))))))
	(else (error "Not compiling non-closure aggregates to Scheme structures" value))))
#;
(define (bound-variables closure)
  (let walk ((tree (closure-formal closure)))
    (cond ((null? tree)
	   '())
	  ((symbol? tree)
	   (list tree))
	  ((pair? tree)
	   (if (eq? (car tree) 'cons)
	       (append (walk (cadr tree))
		       (walk (caddr tree)))
	       (append (walk (car tree))
		       (walk (cdr tree)))))
	  (else
	   (error "Invalid formal parameter tree" (closure-formal closure))))))

(define (procedure-definitions analysis)
  (define (destructuring-let-bindings formal-tree)
    (define (replace old new structure)
      (cond ((eq? structure old) new)
	    ((pair? structure)
	     (cons (replace old new (car structure))
		   (replace old new (cdr structure))))
	    (else structure)))
    (define (xxx part1 part2)
      (append (replace 'the-formals '(car the-formals)
		       (destructuring-let-bindings part1))
	      (replace 'the-formals '(cdr the-formals)
		       (destructuring-let-bindings part2))))
    (cond ((null? formal-tree)
	   '())
	  ((symbol? formal-tree)
	   `((,formal-tree the-formals)))
	  ((pair? formal-tree)
	   (if (eq? (car formal-tree) 'cons)
	       (xxx (cadr formal-tree) (caddr formal-tree))
	       (xxx (car formal-tree) (cdr formal-tree))))))
  (define (maybe-pocedure-definition binding)
    (let ((exp (car binding))
	  (env (cadr binding)))
      (and (pair? exp)
	   (not (eq? (car exp) 'cons))
	   (not (eq? (car exp) 'lambda))
	   (let ((operator (refine-eval-once (car exp) env analysis))
		 (operands (refine-eval-once (cadr exp) env analysis)))
	     (and (closure? operator)
		  (let ((name (call-site->scheme-function-name operator operands)))
		    `(define (,name the-closure the-formals)
		       (let ,(destructuring-let-bindings
			      (car (closure-formal operator)))
			 ,(compile (closure-body operator)
				   (extend-abstract-env
				    (closure-formal operator)
				    operands
				    (closure-env operator))
				   (free-variables
				    `(lambda ,(closure-formal operator)
				       ,(closure-body operator)))
				   analysis)))))))))
  (filter (lambda (x) x)
	  (map maybe-pocedure-definition (analysis-bindings analysis))))

(define (compile-to-scheme program #!optional print-analysis?)
  (set! *symbol-count* 0)
  (let ((analysis (analyze program)))
    (if (and (not (default-object? print-analysis?))
	     print-analysis?)
	(pp analysis))
    `(begin ,@(structure-definitions analysis)
	    ,@(procedure-definitions analysis)
	    ,(compile (macroexpand program)
		      (env->abstract-env (initial-flow-user-env))
		      '()
		      analysis))))
