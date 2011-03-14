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

(define-structure
  (abstract-gensym
   safe-accessors
   (print-procedure
    (simple-unparser-method 'abstract-gensym
     (lambda (abs-gensym)
       (list (abstract-gensym-min abs-gensym)
             (abstract-gensym-max abs-gensym))))))
  min
  max)

(define (trivial-abstract-gensym gensym)
  (make-abstract-gensym (gensym-number gensym) (gensym-number gensym)))

;;; Equality of shapes

(define (abstract-equal? thing1 thing2)
  (cond ((eqv? thing1 thing2)
         #t)
        ((and (abstract-gensym? thing1) (abstract-gensym? thing2))
         (and (= (abstract-gensym-min thing1) (abstract-gensym-min thing2))
              (= (abstract-gensym-max thing1) (abstract-gensym-max thing2))))
        (else (congruent-reduce
               (lambda (lst1 lst2) (every abstract-equal? lst1 lst2))
               thing1
               thing2
               (lambda () #f)))))

(define abstract-hash
  (memoize (make-eq-hash-table)
   (lambda (thing)
     (cond ((abstract-gensym? thing)
            (+ (eqv-hash (abstract-gensym-min thing))
               (eqv-hash (abstract-gensym-max thing))))
           ((or (closure? thing) (pair? thing) (env? thing))
            (object-reduce
             (lambda (lst)
               (equal-hash (map abstract-hash lst)))
             thing))
           (else (eqv-hash thing))))))

(define (abstract-hash-mod thing modulus)
  (modulo (abstract-hash thing) modulus))

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
        ((and (abstract-gensym? thing1) (abstract-gensym? thing2))
         (make-abstract-gensym
          (min (abstract-gensym-min thing1) (abstract-gensym-min thing2))
          (max (abstract-gensym-max thing1) (abstract-gensym-max thing2))))
        (else
         (congruent-map abstract-union thing1 thing2
          (lambda ()
            (error "This program is not union-free:" thing1 thing2))))))
;;;; Things the code generator wants to know about abstract values

;;; Is this shape completely determined by the analysis?
(define (solved-abstractly? thing)
  (cond ((abstract-boolean? thing) #f)
        ((abstract-real? thing) #f)
        ((abstract-none? thing) #f)
        ((abstract-gensym? thing) #f)
        (else
         (object-reduce
          (lambda (lst) (every solved-abstractly? lst))
          thing))))

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
  (cond ((abstract-gensym? thing) #t)
        (else
         (object-reduce
          (lambda (lst) (any depends-on-world? lst))
          thing))))

(define (world-update-value thing old-world new-world)
  (if (or (impossible-world? new-world)
          (impossible-world? old-world)
          (world-equal? old-world new-world))
      thing
      (let loop ((thing thing))
        (cond ((abstract-gensym? thing)
               (make-abstract-gensym
                (world-update-gensym-number (abstract-gensym-min thing) old-world new-world)
                (world-update-gensym-number (abstract-gensym-max thing) old-world new-world)))
              (else (object-map loop thing))))))

(define (world-update-world updatee old-world new-world)
  (if (or (impossible-world? new-world)
          (impossible-world? old-world)
          (impossible-world? updatee))
      updatee
      (make-world
       (+ (world-gensym updatee)
          (- (world-gensym new-world)
             (world-gensym old-world))))))

(define (world-update-gensym-number number old-world new-world)
  (if (< number (world-gensym old-world))
      ;; Already existed
      number
      ;; Newly made
      (+ number (- (world-gensym new-world) (world-gensym old-world)))))
