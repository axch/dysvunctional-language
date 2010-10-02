(declare (usual-integrations))
;;;; Term-rewriting post-processor

;;; This is by no means a general-purpose Scheme code simplifier.  On
;;; the contrary, it is deliberately and heavily specialized to the
;;; task of removing obvious stupidities from the output of the VL
;;; code generator.

;;; Don't worry about the rule-based term-rewriting system that powers
;;; this.  That is its own pile of stuff, good for a few lectures of
;;; Sussman's MIT class Adventures in Advanced Symbolic Programming.
;;; It works, and it's very good for peephole manipulations of
;;; structured expressions (like the output of the VL code generator).

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

(define (symbol-with-prefix? thing prefix)
  (and (symbol? thing)
       (let ((name (symbol->string thing)))
	 (and (> (string-length name) (string-length prefix))
	      (equal? (string-head name (string-length prefix))
		      prefix)))))

(define (record-accessor-name? thing)
  (symbol-with-prefix? thing "closure-"))

(define (generated-temporary? thing)
  ;(symbol-with-prefix? thing "temp-")
  (symbol? thing))

(define post-process-rules
  (list
   (rule (define ((?? line) the-formals)
	   (let (((? name ,symbol?) the-formals))
	     (?? body)))
	 `(define (,@line ,name)
	    ,@body))

   (rule (define (? formals)
	   (let ()
	     (?? body)))
	 `(define ,formals
	    ,@body))

   (rule (let ()
	   (? body))
	 body)

   (rule (begin
	   (? body))
	 body)

   (rule (let ((?? bindings1)
	       ((? name ,generated-temporary?) (cons (? a) (? d)))
	       (?? bindings2))
	   (?? body))
	 `(let (,@bindings1
		,@bindings2)
	    ,@(replace-free-occurrences name `(cons ,a ,d) body)))

   (rule (let ((?? bindings1)
	       ((? name ,generated-temporary?) (? exp ,symbol?))
	       (?? bindings2))
	   (?? body))
	 (and (not (memq exp (append (map car bindings1) (map car bindings2))))
	      `(let (,@bindings1
		     ,@bindings2)
		 ,@(replace-free-occurrences name exp body))))

   (rule (let (((? name ,symbol?) (? exp)))
	   (? name))
	 exp)

   (rule (let ((?? bindings1)
	       ((? name ,symbol?) (? exp))
	       (?? bindings2))
	   (?? body))
	 (and (= 0 (count-free-occurrences name body))
	      `(let (,@bindings1
		     ,@bindings2)
		 ,@body)))

   (rule (car (cons (? a) (? d))) a)
   (rule (cdr (cons (? a) (? d))) d)
   ))

(define post-processor (rule-simplifier post-process-rules))

(define structure-definition->function-definitions-rule
  (rule (define-structure (? name) (?? fields))
	`((define ,(symbol 'make- name) vector)
	  ,@(map (lambda (field index)
		   `(define (,(symbol name '- field) thing)
		      (vector-ref thing ,index)))
		 fields
		 (iota (length fields))))))

(define (structure-definitions->vectors forms)
  (if (list? forms)
      (append-map (lambda (form)
		    (let ((maybe-expansion (structure-definition->function-definitions-rule form)))
		      (if maybe-expansion
			  maybe-expansion
			  (list form))))
		  forms)
      forms))

(define post-inline-rules
  (append
   post-process-rules
   (list
    (rule ((lambda (? names)
	     (?? body))
	   (?? args))
	  `(let ,(map list names args)
	     ,@body))

    (rule (let (((? name ,symbol?) (? exp)))
	    (?? body))
	  (and (not (eq? exp 'the-formals))
	       (= 1 (count-in-tree name body))
	       `(let ()
		  ,@(replace-free-occurrences name exp body))))

    (rule (vector-ref (vector (?? stuff)) (? index ,integer?))
	  (list-ref stuff index))

    )))

(define post-inline (rule-simplifier post-inline-rules))

(define (constructors-only? exp)
  (or (symbol? exp)
      (constant? exp)
      (null? exp)
      (and (pair? exp)
	   (memq (car exp) '(cons vector real))
	   (every constructors-only? (cdr exp)))))

(define inline-constructions
  (rule-simplifier
   (cons
    (rule (let ((?? bindings1)
		((? name ,symbol?) (? exp ,constructors-only?))
		(?? bindings2))
	    (?? body))
	  (and (not (memq exp (append (map car bindings1) (map car bindings2))))
	       `(let (,@bindings1
		      ,@bindings2)
		  ,@(replace-free-occurrences name exp body))))
    post-inline-rules)))

(define sra-cons-definition-rule
  (rule (define ((? name ,symbol?) (?? formals1) (? formal ,symbol?) (?? formals2))
	  (argument-types (?? stuff1) ((? formal) (cons (? car-shape) (? cdr-shape))) (?? stuff2))
	  (?? body))
	(let ((car-name (make-name (symbol formal '-)))
	      (cdr-name (make-name (symbol formal '-)))
	      (index (length formals1))
	      (total-arg-count (+ (length formals1) 1 (length formals2))))
	  (cons (sra-cons-call-site-rule name index total-arg-count)
		`(define (,name ,@formals1 ,car-name ,cdr-name ,@formals2)
		   (argument-types ,@stuff1 (,car-name ,car-shape) (,cdr-name ,cdr-shape) ,@stuff2)
		   (let ((,formal (cons ,car-name ,cdr-name)))
		     ,@body))))))

(define (sra-cons-call-site-rule operation-name replacee-index total-arg-count)
  (rule (,(match:eqv operation-name) (?? args))
	(and (= (length args) total-arg-count)
	     (let ((args1 (take args replacee-index))
		   (arg (list-ref args replacee-index))
		   (args2 (drop args (+ replacee-index 1)))
		   (temp-name (make-name 'temp-)))
	       `(let ((,temp-name ,arg))
		  (,operation-name ,@args1 (car ,temp-name) (cdr ,temp-name) ,@args2))))))

(define (sra-cons forms)
  (if (list? forms)
      (let loop ((forms forms))
	(let scan ((done '())
		   (forms forms))
	  ;(pp `(done ,done forms ,forms))
	  (cond ((null? forms)
		 (reverse done))
		(else
		 (let ((cons-definition-sra-attempt (sra-cons-definition-rule (car forms))))
		   (if cons-definition-sra-attempt
		       (let ((sra-cons-call-site-rule (car cons-definition-sra-attempt))
			     (replacement-form (cdr cons-definition-sra-attempt)))
			 (let ((sra-cons-call-sites (rule-simplifier (list sra-cons-call-site-rule))))
			   (let ((fixed-replacement-form
				  `(,(car replacement-form) ,(cadr replacement-form)
				    ,(caddr replacement-form)
				    ,(sra-cons-call-sites (cadddr replacement-form))))
				 (fixed-done (sra-cons-call-sites (reverse done)))
				 (fixed-forms (sra-cons-call-sites (cdr forms))))
			     (loop (append fixed-done (list fixed-replacement-form) fixed-forms)))))
		       (scan (cons (car forms) done)
			     (cdr forms))))))))
      forms))

(define strip-argument-types
  (rule-simplifier
   (list
    (rule (begin (define-syntax argument-types (?? etc))
		 (?? stuff))
	  `(begin
	     ,@stuff))
    (rule (define (? formals)
	    (argument-types (?? etc))
	    (?? body))
	  `(define ,formals
	     ,@body)))))
