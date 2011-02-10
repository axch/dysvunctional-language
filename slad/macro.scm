(declare (usual-integrations))
;;;; Syntactic preprocessor

;;; Accepts SLAD surface syntax and converts it into the more restricted
;;; language that the SLAD interpreter and the SLAD compiler actually
;;; process.  This is basically a rather straightforward, unhygienic
;;; macro engine; but since SLAD does not allow user-defined macros, the
;;; lack of hygiene is not too bad.

;;; Macro expansion proceeds by structural recursion over the form
;;; being expanded.

(define (macroexpand exp)
  (cond ((constant? exp)
	 exp)
	((variable? exp)
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
	       ((slad-macro? exp)
		(macroexpand (expand-slad-macro exp)))
	       (else
		`(,(macroexpand (car exp))
		  ,(macroexpand-operands (cdr exp))))))
	(else
	 (error "Invalid expression syntax" exp))))

;;; A SLAD procedure application supplying zero or 2+ arguments needs to
;;; be converted into code that CONSes up the right structure and
;;; passes it in.
(define (macroexpand-operands operands)
  (cond ((null? operands)
	 '())
	((null? (cdr operands))
	 (macroexpand (car operands)))
	(else
	 `(cons ,(macroexpand (car operands))
		,(macroexpand-operands (cdr operands))))))

;;; A SLAD procedure definition that specifies zero or 2+ formal
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
	((slad-formal-macro? formals)
	 (macroexpand-formals-macros (expand-slad-formal-macro formals)))
	((pair? formals)
	 (if (eq? (car formals) 'cons)
	     `(cons ,(macroexpand-formals-macros (cadr formals))
		    ,(macroexpand-formals-macros (caddr formals)))
	     `(cons ,(macroexpand-formals-macros (car formals))
		    ,(macroexpand-formals-macros (cdr formals)))))
	(else
	 (error "Invalid formal parameter tree" formals))))

;;; Multi-form procedure bodies are presumed to contain internal
;;; definitions, which are expanded into LETREC.
(define (macroexpand-body forms)
  (let ((definitions (except-last-pair forms))
	(expression (car (last-pair forms))))
    (if (null? definitions)
	expression
	`(letrec ,(map list (map definiendum definitions)
		       (map definiens definitions))
	   ,expression))))

;;;; Additional data-driven macros.

(define *slad-macros* '())

(define (slad-macro? form)
  (memq (car form) (map car *slad-macros*)))

(define (expand-slad-macro form)
  (let ((transformer (assq (car form) *slad-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-slad-macro! name transformer)
  (set! *slad-macros* (cons (cons name transformer) *slad-macros*)))

(define *slad-formal-macros* '())

(define (slad-formal-macro? form)
  (memq (car form) (map car *slad-formal-macros*)))

(define (expand-slad-formal-macro form)
  (let ((transformer (assq (car form) *slad-formal-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-slad-formal-macro! name transformer)
  (set! *slad-formal-macros* (cons (cons name transformer) *slad-formal-macros*)))

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

(define-slad-macro! 'let
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

(define-slad-macro! 'let* let*-transformer)

;;; Following the suggestion in [1], I chose to do IF as a macro
;;; expansion into a primitive IF-PROCEDURE.  This may have been a bad
;;; idea, because the analyzer and code generator seem to need to
;;; special-case applications of this primitive procedure anyway, so
;;; why not leave IF as a special form in the language?
(define-slad-macro! 'if
  (lambda (form)
    `(if-procedure
      ,(cadr form)
      (lambda () ,(caddr form))
      (lambda () ,(cadddr form)))))

;;;; LETREC

;;; LETREC by transformation to the Z combinator.  The Z combinator is
;;; a slightly complexified Y combinator that works in a strict
;;; language.  Also multi-binding LETREC by transformation to an n-ary
;;; version of the Z combinator (this one with an explicit
;;; continuation so it can produce multiple things sensibly).  Do not
;;; read this macro definition unless you are a lambda calculus geek,
;;; and even then I advise looking at some examples of its output
;;; first.

(define (letrec-transformer form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    (cond ((= 0 (length bindings))
	   `(let ()
	      ,@body))
	  ((= 1 (length bindings))
	   (let ((bound-name (caar bindings))
		 (bound-form (cadar bindings)))
	     `(let ((the-Z-combinator
		     (lambda (f)
		       ((lambda (x)
			  (f (lambda (y)
			       ((x x) y))))
			(lambda (x)
			  (f (lambda (y)
			       ((x x) y))))))))
		(let ((,bound-name 
		       (the-Z-combinator
			(lambda (,bound-name)
			  ,bound-form))))
		  ,@body))))
	  (else
	   (let* ((names (map car bindings))
		  (forms (map cadr bindings))
		  (recursive-variants
		   (map (lambda (name-1)
			  `(lambda (y)
			     (Z* ,@names
				 (lambda (,@names)
				   (,name-1 y)))))
			names)))
	     `(letrec ((Z* (lambda (,@names Z*-k)
			     (Z*-k ,@(map (lambda (name)
					    `(,name ,@recursive-variants))
					  names)))))
		(Z* ,@(map (lambda (form)
			     `(lambda (,@names) ,form))
			   forms)
		    (lambda (,@names) ,@body))))))))

(define-slad-macro! 'letrec letrec-transformer)

;;; AND

(define (expand-and form)
  (cond ((null? (cdr form))
	 #t)
	((null? (cddr form))
	 (cadr form))
	(else
	 `(if ,(cadr form)
	      (and ,@(cddr form))
	      #f))))

(define-slad-macro! 'and expand-and)

;;; OR

(define (expand-or form)
  (cond ((null? (cdr form))
	 #f)
	((null? (cddr form))
	 (cadr form))
	(else
	 `(if ,(cadr form)
	      #t
	      (or ,@(cddr form))))))

(define-slad-macro! 'or expand-or)

;;; COND

(define (expand-cond form)
  (define (normalize-else condition)
    (or (eq? condition 'else) condition))
  (cond ((null? (cdr form))
	 0) ; unspecific
	(else
	 `(if ,(normalize-else (caadr form))
	      ,(cadadr form)
	      (cond ,@(cddr form))))))

(define-slad-macro! 'cond expand-cond)

;;; LIST

(define (expand-list form)
  (if (null? (cdr form))
      '()
      `(cons ,(cadr form) (list ,@(cddr form)))))

(define-slad-macro! 'list expand-list)
(define-slad-formal-macro! 'list expand-list)
