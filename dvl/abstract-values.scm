(declare (usual-integrations))
;;;; Abstract values

;;; An abstract value represents the "shape" that a concrete value is
;;; known to explore over the run of the program.  The following
;;; shapes are admitted: Known concrete scalars, the "boolean" shape,
;;; the "real number" shape, pairs of shapes, environments mapping
;;; variables to shapes, and closures with fully-known expressions
;;; (and "environment" shapes for their environments).  There is also
;;; an additional "no information" abstract value, which represents a
;;; thing that is not known to explore any shapes over the run of the
;;; program.  It is an invariant of the analysis that the "no
;;; information" abstract value does not occur inside any other
;;; shapes.

;;; Shapes that have concrete counterparts (concrete scalars, pairs,
;;; environments, and closures) are represented by themselves.  The
;;; remaining three shapes are represented by unique defined objects.
;;; For convenience of comparison, abstract environments are kept in a
;;; canonical form: flattened, restricted to the variables actually
;;; referenced by the closure whose environment it is, and sorted by
;;; the bound names.  The flattening is ok because the language is
;;; pure, so no new bindings are ever added to an environment after it
;;; is first created.

;;; The meaning of a thing having a concrete shape is "This thing is
;;; known to be exactly this value sometimes."  The meaning of the
;;; "boolean" and "real" shapes is "This thing is known to be at least
;;; two distinct such values at some times".  The meaning of the
;;; ABSTRACT-NONE shape is "This thing is not known to ever have a
;;; value".  That only happens when the analysis is progressing.

;;; Unique abstract objects

(define-structure abstract-boolean)
(define abstract-boolean (make-abstract-boolean))
(define (some-boolean? thing)
  (or (boolean? thing)
      (abstract-boolean? thing)))

(define-structure abstract-real)
(define abstract-real (make-abstract-real))
(define (some-real? thing)
  (or (real? thing)
      (abstract-real? thing)))

(define-structure abstract-none)
(define abstract-none (make-abstract-none))

(define-structure (abstract-gensym safe-accessors)
  min
  max)

(define (trivial-abstract-gensym gensym)
  (make-abstract-gensym (gensym-number gensym) (gensym-number gensym)))

;;; Equality of shapes

(define (abstract-equal? thing1 thing2)
  (cond ((eqv? thing1 thing2)
	 #t)
	((and (closure? thing1) (closure? thing2))
	 (and ;; TODO alpha-renaming?
	      (equal? (closure-lambda thing1)
		      (closure-lambda thing2))
	      (abstract-equal? (closure-env thing1)
			       (closure-env thing2))))
	((and (pair? thing1) (pair? thing2))
	 (and (abstract-equal? (car thing1) (car thing2))
	      (abstract-equal? (cdr thing1) (cdr thing2))))
	((and (env? thing1) (env? thing2))
	 (abstract-equal? (env-bindings thing1)
			  (env-bindings thing2)))
	((and (abstract-gensym? thing1) (abstract-gensym? thing2))
	 (and (= (abstract-gensym-min thing1) (abstract-gensym-min thing2))
	      (= (abstract-gensym-max thing1) (abstract-gensym-max thing2))))
	(else #f)))

(define (abstract-hash-mod thing modulus)
  (cond ((closure? thing)
	 (modulo (+ (equal-hash-mod (closure-body thing) modulus)
		    (equal-hash-mod (closure-formal thing) modulus)
		    (abstract-hash-mod (closure-env thing) modulus))
		 modulus))
	((pair? thing)
	 (modulo (+ (abstract-hash-mod (car thing) modulus)
		    (abstract-hash-mod (cdr thing) modulus))
		 modulus))
	((env? thing)
	 (abstract-hash-mod (env-bindings thing) modulus))
	((abstract-gensym? thing)
	 (modulo (+ (abstract-hash-mod (abstract-gensym-min thing) modulus)
		    (abstract-hash-mod (abstract-gensym-max thing) modulus))
		 modulus))
	(else (eqv-hash-mod thing modulus))))

(define make-abstract-hash-table
  (strong-hash-table/constructor abstract-hash-mod abstract-equal? #t))

(define (abstract-value-superset? thing1 thing2)
  (let ((unique (list 'unique))
	(union (abstract-union thing1 thing2 (lambda () unique))))
    (and (not (eq? unique union))
	 (abstract-equal? thing1 union))))

;;; Union of shapes --- can be either this or that.  ABSTRACT-UNION is
;;; only used on the return values of IF statements.

(define (abstract-union thing1 thing2 #!optional lose)
  (cond ((abstract-equal? thing1 thing2)
	 thing1)
	((abstract-none? thing1)
	 thing2)
	((abstract-none? thing2)
	 thing1)
	((and (some-boolean? thing1) (some-boolean? thing2))
	 abstract-boolean)
	((and (some-real? thing1) (some-real? thing2))
	 abstract-real)
	((and (closure? thing1) (closure? thing2)
	      (equal? (closure-lambda thing1)
		      (closure-lambda thing2)))
	 (make-closure (closure-formal thing1)
		       (closure-body thing1)
		       (abstract-union (closure-env thing1)
				       (closure-env thing2))))
	((and (pair? thing1) (pair? thing2))
	 (cons (abstract-union (car thing1) (car thing2))
	       (abstract-union (cdr thing1) (cdr thing2))))
	((and (env? thing1)
	      (env? thing2))
	 (make-env
	  (abstract-union (env-bindings thing1)
			  (env-bindings thing2))))
	((and (abstract-gensym? thing1) (abstract-gensym? thing2))
	 (make-abstract-gensym
	  (min (abstract-gensym-min thing1) (abstract-gensym-min thing2))
	  (max (abstract-gensym-max thing1) (abstract-gensym-max thing2))))
	(else
	 (if (default-object? lose)
	     (error "This program is not union-free:" thing1 thing2)
	     (lose)))))
;;;; Things the code generator wants to know about abstract values

;;; Is this shape completely determined by the analysis?
(define (solved-abstractly? thing)
  (cond ((null? thing) #t)
	((boolean? thing) #t)
	((real? thing) #t)
	((abstract-boolean? thing) #f)
	((abstract-real? thing) #f)
	((abstract-none? thing) #f)
	((primitive? thing) #t)
	((closure? thing) (solved-abstractly? (closure-env thing)))
	((pair? thing) (and (solved-abstractly? (car thing))
			    (solved-abstractly? (cdr thing))))
	((env? thing)
	 (every solved-abstractly? (map cdr (env-bindings thing))))
	((abstract-gensym? thing) #f)
	(else (error "Invalid abstract value" thing))))

;;; If so, what's the Scheme code to make that value?
(define (solved-abstract-value->constant thing)
  (cond ((or (null? thing) (boolean? thing) (real? thing)) thing)
	((primitive? thing) (primitive-name thing))
	((pair? thing)
	 (list 'cons (solved-abstract-value->constant (car thing))
	       (solved-abstract-value->constant (cdr thing))))
	(else '(vector))))

;;; What variables in this expression need determining at runtime?
(define (interesting-variables exp env)
  (define (interesting-variable? var)
    (not (solved-abstractly? (lookup var env))))
  (sort (filter interesting-variable? (free-variables exp))
	symbol<?))

;;; What type does this shape represent?
(define (shape->type-declaration thing)
  (define (interesting-environment-values closure)
    (let ((vars (closure-free-variables closure))
	  (env (closure-env closure)))
      (map (lambda (var) (lookup var env))
	   (interesting-variables vars env))))
  (cond ((abstract-real? thing) 'real)
	((abstract-boolean? thing) 'boolean)
	((solved-abstractly? thing) '(vector))
	((abstract-gensym? thing) 'gensym)
	((closure? thing)
	 (cons (abstract-closure->scheme-structure-name thing)
	       (map shape->type-declaration
		    (interesting-environment-values thing))))
	((pair? thing)
	 `(cons ,(shape->type-declaration (car thing))
		,(shape->type-declaration (cdr thing))))
	(else (error "shape->type-declaration loses!" thing))))

(define (depends-on-world? thing)
  (cond ((null? thing) #f)
	((boolean? thing) #f)
	((real? thing) #f)
	((abstract-boolean? thing) #f)
	((abstract-real? thing) #f)
	((abstract-none? thing) #f)
	((primitive? thing) #f)
	((closure? thing) (depends-on-world? (closure-env thing)))
	((pair? thing) (or (depends-on-world? (car thing))
			   (depends-on-world? (cdr thing))))
	((env? thing)
	 (any depends-on-world? (map cdr (env-bindings thing))))
	((abstract-gensym? thing) #t)
	(else (error "Invalid abstract value" thing))))

(define (world-update-value thing old-world new-world)
  (if (or (impossible-world? new-world)
	  (impossible-world? old-world))
      thing
      (let loop ((thing thing))
	(cond ((null? thing) thing)
	      ((boolean? thing) thing)
	      ((real? thing) thing)
	      ((abstract-boolean? thing) thing)
	      ((abstract-real? thing) thing)
	      ((abstract-none? thing) thing)
	      ((primitive? thing) thing)
	      ((closure? thing)
	       (make-closure
		(closure-lambda thing)
		(loop (closure-env thing))))
	      ((pair? thing) (make-dvl-pair (loop (car thing))
					    (loop (cdr thing))))
	      ((env? thing)
	       (make-env (map cons
			      (map car (env-bindings thing))
			      (map loop (map cdr (env-bindings thing))))))
	      ((eq? the-abstract-gensym thing) thing)
	      ((abstract-gensym? thing)
	       (let ((difference
		      (- (world-gensym new-world) (world-gensym old-world))))
		 (make-abstract-gensym
		  (+ (abstract-gensym-min thing) difference)
		  (+ (abstract-gensym-max thing) difference))))
	      (else (error "Invalid abstract value" thing))))))

(define (world-update-world updatee old-world new-world)
  (if (or (impossible-world? new-world)
	  (impossible-world? old-world)
	  (impossible-world? updatee))
      updatee
      (make-world
       (world-io-version updatee)
       (+ (world-gensym updatee)
	  (- (world-gensym new-world)
	     (world-gensym old-world))))))
