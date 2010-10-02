(declare (usual-integrations))
;;;; Syntactic preprocessor

;;; Accepts VL surface syntax and converts it into the more restricted
;;; language that the VL interpreter and the VL compiler actually
;;; process.  This is basically a rather straightforward, unhygienic
;;; macro engine; but since VL does not allow user-defined macros, the
;;; lack of hygiene is not too bad.

;;; Macro expansion proceeds by structural recursion over the form
;;; being expanded.

(define (macroexpand exp)
  (cond ((variable? exp)
	 exp)
	((null? exp)
	 '())
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		`(lambda ,(macroexpand-formals (cadr exp))
		   ,(macroexpand (caddr exp))))
	       ((eq? (car exp) 'cons)
		`(cons ,(macroexpand (cadr exp))
		       ,(macroexpand (caddr exp))))
	       ((vl-macro? exp)
		(macroexpand (expand-vl-macro exp)))
	       (else
		`(,(macroexpand (car exp))
		  ,(macroexpand-operands (cdr exp))))))
	(else
	 (error "Invalid expression syntax" exp))))

;;; A VL procedure application supplying zero or 2+ arguments needs to
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

;;; A VL procedure definition that specifies zero or 2+ formal
;;; parameters needs to be converted into one that specifies the right
;;; destructuring.  While I'm at it, I can allow explicit
;;; destructuring in the surface syntax.
(define (macroexpand-formals formals)
  (cond ((null? formals)
	 (list formals))
	((and (pair? formals) (null? (cdr formals)))
	 formals)
	((pair? formals)
	 (list
	  (let walk ((formals (apply cons* formals)))
	    (cond ((symbol? formals) formals)
		  ((null? formals) formals)
		  ((pair? formals)
		   (if (eq? (car formals) 'cons)
		       `(cons ,(walk (cadr formals))
			      ,(walk (caddr formals)))
		       `(cons ,(walk (car formals))
			      ,(walk (cdr formals)))))
		  (else
		   (error "Invalid formal parameter tree" formals))))))
	(else
	 (error "Invalid formal parameter tree" formals))))

;;;; Additional data-driven macros.

(define *vl-macros* '())

(define (vl-macro? form)
  (memq (car form) (map car *vl-macros*)))

(define (expand-vl-macro form)
  (let ((transformer (assq (car form) *vl-macros*)))
    (if transformer
	((cdr transformer) form)
	(error "Undefined macro" form))))

(define (define-vl-macro! name transformer)
  (set! *vl-macros* (cons (cons name transformer) *vl-macros*)))

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

(define-vl-macro! 'let
  (lambda (form)
    (if (symbol? (cadr form))
	(named-let-transformer form)
	(normal-let-transformer form))))

;;; Following the suggestion in [1], I chose to do IF as a macro
;;; expansion into a primitive IF-PROCEDURE.  This may have been a bad
;;; idea, because the analyzer and code generator seem to need to
;;; special-case applications of this primitive procedure anyway, so
;;; why not leave IF as a special form in the language?
(define-vl-macro! 'if
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
	   (let ((names (map car bindings))
		 (forms (map cadr bindings)))
	     (define (recursive-variant name)
	       `(,name ,@(map (lambda (name-1)
				`(lambda (y)
				   (Z* ,@names
				       (lambda (,@names)
					 (,name-1 y)))))
			      names)))
	     `(letrec ((Z* (lambda (,@names Z*-k)
			     (Z*-k ,@(map recursive-variant names)))))
		(Z* ,@(map (lambda (form)
			     `(lambda (,@names) ,form))
			   forms)
		    (lambda (,@names) ,@body))))))))

(define-vl-macro! 'letrec letrec-transformer)
