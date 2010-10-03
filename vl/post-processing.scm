(declare (usual-integrations))
;;;; Post processing

;;; The post-processing stage consists of several sub-stages, which
;;; can in principle be mixed and matched to produce various
;;; combinations of how cleaned-up one wants the output to be.
;;; We have:
;;; - A local, term-rewriting cleanup device
;;; - A frob that replace define-structure with explicit vectors
;;; - Scalar replacement of aggregates
;;; - Removal of argument-type annotations, if they have been
;;;   emitted by the code generator
;;; - Inliner of function definitions, which inlines everything
;;;   that doesn't call itself
;;; - Term-rewriting post-inline cleanup
;;; - A frob that pushes constructor invocations from let bindings
;;;   and into their use sites.

;;; Don't worry about the rule-based term-rewriting system that powers
;;; this.  That is its own pile of stuff, good for a few lectures of
;;; Sussman's MIT class Adventures in Advanced Symbolic Programming.
;;; It works, and it's very good for peephole manipulations of
;;; structured expressions (like the output of the VL code generator).
;;; If you really want to see it, though, it's included in
;;; support/rule-system.

;;; The rules below consist of a pattern to try to match and an
;;; expression to evaluate to compute a replacement for that match
;;; should a match be found.  Patterns match themselves; the construct
;;; (? name) introduces a pattern variable named name; the construct
;;; (? name ,predicate) is a restrcted pattern variable which only
;;; matches things the predicate accepts; the construct (?? name)
;;; introduces a sublist pattern variable.  The replacement expression
;;; is evaluated in an environment where the pattern variables are
;;; bound to the things they matched.  The rules are applied to every
;;; subexpression of the input expression repeatedly until the result
;;; settles down.

(define (prettify-compiler-output output)
  (tidy
   (inline
    (strip-argument-types
     (scalar-replace-aggregates
      (structure-definitions->vectors
       (tidy
	output)))))))

(define (compile-to-pretty-scheme program)
  (prettify-compiler-output
   (compile-to-scheme program #t)))

;;;; Term-rewriting tidier

;;; This is by no means a general-purpose Scheme code simplifier.  On
;;; the contrary, it is deliberately and heavily specialized to the
;;; task of removing obvious stupidities from the output of the VL
;;; code generator.

(define (symbol-with-prefix? thing prefix)
  (and (symbol? thing)
       (let ((name (symbol->string thing)))
	 (and (> (string-length name) (string-length prefix))
	      (equal? (string-head name (string-length prefix))
		      prefix)))))

(define (generated-temporary? thing)
  (symbol-with-prefix? thing "temp-"))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
	   (memq (car exp) '(cons vector real))
	   (every constructors-only? (cdr exp)))))

(define tidy-rules
  (list

   (rule `(let ()
	    (? body))
	 body)

   (rule `(begin
	    (? body))
	 body)

   (rule `((lambda (? names)
	     (?? body))
	   (?? args))
	 `(let ,(map list names args)
	    ,@body))

   (rule `(car (cons (? a) (? d))) a)
   (rule `(cdr (cons (? a) (? d))) d)
   (rule `(vector-ref (vector (?? stuff)) (? index ,integer?))
	 (list-ref stuff index))

   (rule `(let (((? name ,symbol?) (? exp)))
	    (? name))
	 exp)

   (rule `(let ((?? bindings1)
		((? name ,symbol?) (? exp))
		(?? bindings2))
	    (?? body))
	 (and (= 0 (count-free-occurrences name body))
	      `(let (,@bindings1
		     ,@bindings2)
		 ,@body)))

   (rule `(let ((?? bindings1)
		((? name ,symbol?) (? exp))
		(?? bindings2))
	    (?? body))
	 (and (= 1 (count-free-occurrences name body))
	      (not (memq exp (append (map car bindings1) (map car bindings2))))
	      `(let (,@bindings1
		     ,@bindings2)
		 ,@(replace-free-occurrences name exp body))))

   (rule `(let ((?? bindings1)
		((? name ,symbol?) (? exp ,constructors-only?))
		(?? bindings2))
	    (?? body))
	 (and (not (memq exp (append (map car bindings1) (map car bindings2))))
	      `(let (,@bindings1
		     ,@bindings2)
		 ,@(replace-free-occurrences name exp body))))

   (rule `(let ((?? bindings1)
		((? name ,generated-temporary?) (cons (? a) (? d)))
		(?? bindings2))
	    (?? body))
	 `(let (,@bindings1
		,@bindings2)
	    ,@(replace-free-occurrences name `(cons ,a ,d) body)))

   ))

(define tidy (rule-simplifier tidy-rules))

;;;; Turning record structures into vectors

(define (structure-definition? form)
  (and (pair? form)
       (eq? (car form) 'define-structure)))

(define (expand-if-structure-definition form)
  (if (structure-definition? form)
      (let ((name (cadr form))
	    (fields (cddr form)))
	`((define ,(symbol 'make- name) vector)
	  ,@(map (lambda (field index)
		   `(define (,(symbol name '- field) thing)
		      (vector-ref thing ,index)))
		 fields
		 (iota (length fields)))))
      (list form)))

(define (structure-definitions->vectors forms)
  (if (list? forms)
      (let ((structure-names
	     (map cadr (filter structure-definition? forms))))
	(define (fix-argument-types forms)
	  (let loop ((forms forms)
		     (structure-names structure-names))
	    (if (null? structure-names)
		forms
		(loop (replace-free-occurrences (car structure-names) 'vector forms)
		      (cdr structure-names)))))
	(fix-argument-types
	 (append-map expand-if-structure-definition forms)))
      forms))

;;;; Scalar replacement of aggregates

;;; What a mess!

(define (call-site-replacement temp-name constructor-type count)
  (if (eq? 'cons constructor-type)
      `((car ,temp-name) (cdr ,temp-name))
      (map (lambda (index)
	     `(vector-ref ,temp-name ,index))
	   (iota count))))

(define (cons-or-vector? thing)
  (or (eq? thing 'cons)
      (eq? thing 'vector)))

(define sra-definition-rule
  (rule `(define ((? name ,symbol?) (?? formals1) (? formal ,symbol?) (?? formals2))
	  (argument-types (?? stuff1) ((? formal) ((? constructor ,cons-or-vector?) (?? arg-piece-shapes))) (?? stuff2))
	  (?? body))
	(let ((arg-piece-names (map (lambda (shape)
				      (make-name (symbol formal '-)))
				    arg-piece-shapes))
	      (index (length formals1))
	      (total-arg-count (+ (length formals1) 1 (length formals2))))
	  (cons (sra-call-site-rule name constructor index (length arg-piece-shapes) total-arg-count)
		`(define (,name ,@formals1 ,@arg-piece-names ,@formals2)
		   (argument-types ,@stuff1 ,@(map list arg-piece-names arg-piece-shapes) ,@stuff2)
		   (let ((,formal (,constructor ,@arg-piece-names)))
		     ,@body))))))

(define (sra-call-site-rule operation-name constructor replacee-index num-replacees total-arg-count)
  (rule `(,operation-name (?? args))
	(and (= (length args) total-arg-count)
	     (let ((args1 (take args replacee-index))
		   (arg (list-ref args replacee-index))
		   (args2 (drop args (+ replacee-index 1)))
		   (temp-name (make-name 'temp-)))
	       `(let ((,temp-name ,arg))
		  (,operation-name ,@args1 ,@(call-site-replacement temp-name constructor num-replacees) ,@args2))))))

(define (non-repeating-rule-simplifier the-rules)
  (define (simplify-expression expression)
    (let ((subexpressions-simplified
	   (if (list? expression)
	       (map simplify-expression expression)
	       expression)))
      (try-rules subexpressions-simplified the-rules
       (lambda (result fail)
	 result)
       (lambda ()
	 subexpressions-simplified))))
  (rule-memoize simplify-expression))

(define (scalar-replace-aggregates forms)
  (define (try-defining-rule rule target done rest loop lose)
    (let ((definition-sra-attempt (rule target)))
      (if definition-sra-attempt
	  (let ((sra-call-site-rule (car definition-sra-attempt))
		(replacement-form (cdr definition-sra-attempt)))
	    (let ((sra-call-sites (non-repeating-rule-simplifier (list sra-call-site-rule))))
	      (let ((fixed-replacement-form
		     `(,(car replacement-form) ,(cadr replacement-form)
		       ,(caddr replacement-form)
		       ,(sra-call-sites (cadddr replacement-form))))
		    (fixed-done (sra-call-sites (reverse done)))
		    (fixed-forms (sra-call-sites rest)))
		(loop (append fixed-done (list fixed-replacement-form) fixed-forms)))))
	  (lose))))
  (if (list? forms)
      (let loop ((forms forms))
	(let scan ((done '())
		   (forms forms))
	  ;(pp `(done ,done forms ,forms))
	  (cond ((null? forms)
		 (reverse done))
		(else
		 (try-defining-rule sra-definition-rule (car forms) done (cdr forms) loop
		  (lambda ()
		    (scan (cons (car forms) done) (cdr forms))))))))
      forms))

(define strip-argument-types
  (rule-simplifier
   (list
    (rule `(begin (define-syntax argument-types (?? etc))
		  (?? stuff))
	  `(begin
	     ,@stuff))
    (rule `(define (? formals)
	     (argument-types (?? etc))
	     (?? body))
	  `(define ,formals
	     ,@body)))))

;;;; Inlining procedure definitions

;;; Do this after stripping out argument-types declarations, because
;;; the things that handle them expect procedures not to be inlined
;;; yet.

(define (inline forms)
  (define (definition? form)
    (and (pair? form)
	 (eq? (car form) 'define)))
  (define (definiend definition)
    (if (pair? (cadr definition))
	(caadr definition)
	(cadr definition)))
  (define ((defines-name? name) form)
    (and (definition? form)
	 (eq? name (definiend form))))
  (define (definition-expression definition)
    (if (pair? (cadr definition))
	`(lambda ,(cdadr definition)
	   ,@(cddr definition))
	(caddr definition)))
  (define (non-self-calling? definition)
    (= 0 (count-in-tree (definiend definition) (definition-expression definition))))
  (define (inline-defn defn others)
    (replace-free-occurrences (definiend defn) (definition-expression defn) others))
  (if (list? forms)
      (let loop ((forms forms))
	(let scan ((done '())
		   (forms forms))
	  (cond ((null? forms)
		 (reverse done))
		((and (definition? (car forms))
		      (non-self-calling? (car forms)))
		 (let ((defn (car forms))
		       (others (append (reverse done) (cdr forms))))
		   ;; Can insert other inlining restrictions here
		   (loop (inline-defn defn others))))
		(else
		 (scan (cons (car forms) done)
		       (cdr forms))))))
      forms))
