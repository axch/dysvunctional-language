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

;;; Equality of shapes

(define (abstract-equal? thing1 thing2)
  (cond ((eqv? thing1 thing2)
	 #t)
	((and (closure? thing1) (closure? thing2))
	 (and ;; TODO alpha-renaming?
	      (equal? (closure-expression thing1)
		      (closure-expression thing2))
	      (abstract-equal? (closure-env thing1)
			       (closure-env thing2))))
	((and (pair? thing1) (pair? thing2))
	 (and (abstract-equal? (car thing1) (car thing2))
	      (abstract-equal? (cdr thing1) (cdr thing2))))
	((and (env? thing1) (env? thing2))
	 (abstract-equal? (env-bindings thing1)
			  (env-bindings thing2)))
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
	(else (eqv-hash-mod thing modulus))))

(define make-abstract-hash-table
  (strong-hash-table/constructor abstract-hash-mod abstract-equal? #t))

;;; Union of shapes --- can be either this or that.  ABSTRACT-UNION is
;;; only used on the return values of IF statements.

(define (abstract-union thing1 thing2)
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
	      (equal? (closure-expression thing1)
		      (closure-expression thing2)))
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
	(else
	 (error "This program is not union-free:" thing1 thing2))))

;;; Is this shape completely determined?

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
	(else
	 (error "Invalid abstract value" thing))))

(define (solved-abstract-value->constant thing)
  (cond ((or (null? thing) (boolean? thing) (real? thing))
	 thing)
	((primitive? thing) (primitive-name thing))
	((pair? thing)
	 (list 'cons (solved-abstract-value->constant (car thing))
	       (solved-abstract-value->constant (cdr thing))))
	(else '(vector))))

(define (interesting-variable? env)
  (lambda (var)
    (not (solved-abstractly? (lookup var env)))))

(define (interesting-variables exp env)
  (sort (filter (interesting-variable? env)
		(free-variables exp))
	symbol<?))

(define (shape->type-declaration thing)
  (cond ((abstract-real? thing)
	 'real)
	((abstract-boolean? thing)
	 'boolean)
	((solved-abstractly? thing)
	 '(vector))
	((closure? thing)
	 (cons (abstract-closure->scheme-structure-name thing)
	       (map shape->type-declaration
		    (interesting-environment-values thing))))
	((pair? thing)
	 `(cons ,(shape->type-declaration (car thing))
		,(shape->type-declaration (cdr thing))))
	(else
	 (error "shape->type-declaration loses!" thing))))

(define (interesting-environment-values closure)
  (let ((vars (closure-free-variables closure))
	(env (closure-env closure)))
    (map (lambda (var)
	   (lookup var env))
	 (interesting-variables vars env))))
