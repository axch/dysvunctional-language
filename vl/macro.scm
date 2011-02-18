(declare (usual-integrations))
;;;; Syntactic preprocessor

;;; Accepts somewhat Schemey surface syntax and converts it into a
;;; more restricted language for subsequent processing.  This is
;;; basically a rather straightforward, unhygienic macro engine; but
;;; since we do not allow user-defined macros, the lack of hygiene is
;;; not too bad.

;;; Macro expansion proceeds by structural recursion over the form
;;; being expanded.

(define (macroexpand exp)
  (cond ((or (constant? exp) (variable? exp))
	 exp)
	((null? exp)
	 '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		`(lambda ,(macroexpand-formals (cadr exp))
		   ,(macroexpand (macroexpand-body (cddr exp)))))
	       ((eq? (car exp) 'cons)
		`(cons ,(macroexpand (cadr exp))
		       ,(macroexpand (caddr exp))))
	       ((exp-macro? exp)
		(macroexpand (expand-exp-macro exp)))
	       (else
		`(,(macroexpand (car exp))
		  ,(macroexpand-operands (cdr exp))))))
	(else
	 (error "Invalid expression syntax" exp))))

;;; A surface procedure application supplying zero or 2+ arguments
;;; needs to be converted into code that CONSes up the right structure
;;; and passes it in.
(define (macroexpand-operands operands)
  (cond ((null? operands)
	 '())
	((null? (cdr operands))
	 (macroexpand (car operands)))
	(else
	 `(cons ,(macroexpand (car operands))
		,(macroexpand-operands (cdr operands))))))

;;; A surface procedure definition that specifies zero or 2+ formal
;;; parameters needs to be converted into one that specifies the right
;;; destructuring.  While I'm at it, I can allow explicit
;;; destructuring in the surface syntax.
(define (macroexpand-formals formals)
  (cond ((null? formals)
	 (list formals))
	((and (pair? formals) (null? (cdr formals)))
	 (list (macroexpand-formals-macros (car formals))))
	((pair? formals)
	 (list (macroexpand-formals-macros (apply cons* formals))))
	(else
	 (error "Invalid formal parameter tree" formals))))

(define (macroexpand-formals-macros formals)
  (cond ((symbol? formals) formals)
	((null? formals) formals)
	((formal-macro? formals)
	 (macroexpand-formals-macros (expand-formal-macro formals)))
	((pair? formals)
	 (if (eq? (car formals) 'cons)
	     `(cons ,(macroexpand-formals-macros (cadr formals))
		    ,(macroexpand-formals-macros (caddr formals)))
	     `(cons ,(macroexpand-formals-macros (car formals))
		    ,(macroexpand-formals-macros (cdr formals)))))
	(else
	 (error "Invalid formal parameter tree" formals))))

;;; A surface procedure body must be a single expression, except that
;;; this expression may be preceded by internal definitions.  The
;;; latter case is converted into LETREC.
(define (macroexpand-body forms)
  (let ((definitions (except-last-pair forms))
	(expression (car (last-pair forms))))
    (if (null? definitions)
	expression
	`(letrec ,(map list (map definiendum definitions)
		       (map definiens definitions))
	   ,expression))))

;;;; Additional data-driven macros.

(define *exp-macros* '())

(define (exp-macro? form)
  (memq (car form) (map car *exp-macros*)))

(define (expand-exp-macro form)
  (let ((transformer (assq (car form) *exp-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-exp-macro! name transformer)
  (set! *exp-macros* (cons (cons name transformer) *exp-macros*)))

;;; A also want to allow "macros", such as LIST and CONS*, in the
;;; surface syntax of formal parameters, but I do not want any old
;;; expression macro to be expanded in a formal parameter context.

(define *formal-macros* '())

(define (formal-macro? form)
  (memq (car form) (map car *formal-macros*)))

(define (expand-formal-macro form)
  (let ((transformer (assq (car form) *formal-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-formal-macro! name transformer)
  (set! *formal-macros* (cons (cons name transformer) *formal-macros*)))

;;; LET
(define (normal-let-transformer form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    `((lambda ,(map car bindings)
	,@body)
      ,@(map cadr bindings))))

;;; Named LET
(define (named-let-transformer form)
  (let ((name (cadr form))
	(bindings (caddr form))
	(body (cdddr form)))
    `(letrec ((,name (lambda ,(map car bindings)
		       ,@body)))
       (,name ,@(map cadr bindings)))))

(define-exp-macro! 'let
  (lambda (form)
    (if (symbol? (cadr form))
	(named-let-transformer form)
	(normal-let-transformer form))))

;;; LET*
(define (let*-transformer form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    (if (null? bindings)
	`(let ()
	   ,@body)
	`(let (,(car bindings))
	   (let* ,(cdr bindings)
	     ,@body)))))
(define-exp-macro! 'let* let*-transformer)

;;; There are many ways to do IF.  I chose to expand IF into a call to
;;; a primitive IF-PROCEDURE.  This primitive is unusual because it
;;; accepts and internally calls compound procedures.
(define-exp-macro! 'if
  (lambda (form)
    `(if-procedure
      ,(cadr form)
      (lambda () ,(caddr form))
      (lambda () ,(cadddr form)))))

;;;; LETREC

(define (letrec-transformer form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    (cond ((= 0 (length bindings))
	   `(let () ,@body))
	  ((= 1 (length bindings))
	   (unary-letrec bindings body))
	  (else
	   (nary-letrec bindings body)))))

(define-exp-macro! 'letrec letrec-transformer)

;;; Unary LETREC by transformation to the Z combinator.  The Z
;;; combinator is a slightly complexified Y combinator that works in a
;;; strict language.

(define (unary-letrec bindings body)
  (let ((bound-name (caar bindings))
	(bound-form (cadar bindings)))
    `(let ((the-Z-combinator
	    (lambda (kernel)
	      ((lambda (recur)
		 (kernel (lambda (y) ((recur recur) y))))
	       (lambda (recur)
		 (kernel (lambda (y) ((recur recur) y))))))))
       (let ((,bound-name
	      (the-Z-combinator
	       (lambda (,bound-name)
		 ,bound-form))))
	 ,@body))))

;;; Multi-binding LETREC by transformation to an n-ary version of the
;;; Z combinator (this one with an explicit continuation so it can
;;; produce multiple things sensibly), itself defined via a unary
;;; LETREC.  Do not read this macro definition unless you are a lambda
;;; calculus geek, and even then I advise looking at some examples of
;;; its output first.  If you grok what this does and are worried
;;; about the performance implications, go read letrec.scm.

(define (nary-letrec bindings body)
  (let ((names (map car bindings))
	(forms (map cadr bindings)))
    `(letrec ((Z* (lambda (Z*-k kernels)
		    (let ((recursive-variants
			   (cons*
			    ,@(map (lambda (name-1)
				     `(lambda (y)
					(Z* (lambda (,@names)
					      (,name-1 y))
					    kernels)))
				   names))))
		      (let ((,(apply cons* names) kernels))
			(Z*-k ,@(map (lambda (name)
				       `(,name recursive-variants))
				     names)))))))
       (Z* (lambda (,@names) ,@body)
	   ,@(map (lambda (form)
		    `(lambda (,@names) ,form))
		  forms)))))

;;; AND

(define (expand-and form)
  (cond ((null? (cdr form))  #t)
	((null? (cddr form)) (cadr form))
	(else
	 `(if ,(cadr form)
	      (and ,@(cddr form))
	      #f))))
(define-exp-macro! 'and expand-and)

;;; OR

(define (expand-or form)
  (cond ((null? (cdr form))  #f)
	((null? (cddr form)) (cadr form))
	(else
	 `(if ,(cadr form)
	      #t
	      (or ,@(cddr form))))))
(define-exp-macro! 'or expand-or)

;;; COND

(define (expand-cond form)
  (define (normalize-else condition)
    (or (eq? condition 'else) condition))
  (if (null? (cdr form))
      0	; unspecific
      `(if ,(normalize-else (caadr form))
	   ,(cadadr form)
	   (cond ,@(cddr form)))))
(define-exp-macro! 'cond expand-cond)

;;; LIST

(define (expand-list form)
  (if (null? (cdr form))
      '()
      `(cons ,(cadr form) (list ,@(cddr form)))))
(define-exp-macro! 'list expand-list)
(define-formal-macro! 'list expand-list)

;;; CONS*

(define (expand-cons* form)
  (cond ((null? (cdr form))  '())
	((null? (cddr form)) (cadr form))
	(else
	 `(cons ,(cadr form) (cons* ,@(cddr form))))))
(define-exp-macro! 'cons* expand-cons*)
(define-formal-macro! 'cons* expand-cons*)
