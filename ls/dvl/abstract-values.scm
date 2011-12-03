(declare (usual-integrations))
;;;; Abstract values

;;; An abstract value represents the "shape" that a concrete value is
;;; known to explore over the run of the program.  VL admits the
;;; following shapes: Known concrete scalars, the "boolean" shape, the
;;; "real number" shape, pairs of shapes, environments mapping
;;; variables to shapes, and closures with fully-known expressions
;;; (and "environment" shapes for their environments).  There is also
;;; an additional "no value" abstract value, which represents a thing
;;; that is not known to explore any shapes over the run of the
;;; program.  It is an invariant of the analysis that the "no value"
;;; abstract value does not occur inside any other shapes.  DVL
;;; extends the list of shapes by the "gensym" shape.

;;; Shapes that have concrete counterparts (concrete scalars, pairs,
;;; environments, and closures) are represented by themselves.  The
;;; remaining three shapes are represented by unique defined objects.
;;; For convenience of comparison, abstract environments are kept in a
;;; canonical form: flattened, restricted to the variables actually
;;; referenced by the closure whose environment it is, and sorted by
;;; the bound names.  The flattening is ok because no new bindings are
;;; ever added to an environment after it is first created.

;;; The meaning of a thing having a concrete shape is "This thing is
;;; known to be exactly this value sometimes."  The meaning of the
;;; "boolean", "real", and "gensym" shapes is "This thing is known to
;;; be at least two distinct such values at some times".  The meaning
;;; of the ABSTRACT-NONE shape is "This thing is not known to ever
;;; have a value".  That only happens when the analysis is
;;; progressing.

;;; Unique abstract objects

(define-structure (abstract-boolean safe-accessors))
(define abstract-boolean (make-abstract-boolean))
(define (some-boolean? thing)
  (or (boolean? thing)
      (abstract-boolean? thing)))

(define-structure (abstract-real safe-accessors))
(define abstract-real (make-abstract-real))
(define (some-real? thing)
  (or (real? thing)
      (abstract-real? thing)))

(define-structure (abstract-none safe-accessors))
(define abstract-none (make-abstract-none))

;;; The "gensym" shape is represented by a pair of integers MIN and
;;; MAX, the minimum and maximum values of the gensym number inside
;;; the "gensym" shape a thing is known to be.  Alternatively, one
;;; could store inside the "gensym" shape a set of gensym numbers a
;;; thing is known to be.  Obviously, the former representation is
;;; more efficient (and is sufficient for the purpose of the dataflow
;;; analysis).  However, the latter interpretation can sometimes be
;;; more illuminating.
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

;;; A trivial abstract gensym is the one corresponding to a singleton
;;; set of gensym numbers a thing is known to be.
(define (trivial-abstract-gensym gensym)
  (make-abstract-gensym (gensym-number gensym) (gensym-number gensym)))

;;; Equality of shapes

(define (abstract-equal? thing1 thing2)
  (cond ((eqv? thing1 thing2)
         #t)
        ((and (abstract-gensym? thing1) (abstract-gensym? thing2))
         (and (= (abstract-gensym-min thing1) (abstract-gensym-min thing2))
              (= (abstract-gensym-max thing1) (abstract-gensym-max thing2))))
        ((and (gensym? thing1) (gensym? thing2))
         (= (gensym-number thing1) (gensym-number thing2)))
        (else (congruent-reduce
               (lambda (lst1 lst2) (every abstract-equal? lst1 lst2))
               thing1
               thing2
               (lambda () #f)))))

(define (caches-abstract-hash? thing)
  (or (env? thing) (closure? thing) (binding? thing)))

(define (cached-abstract-hash thing)
  (cond ((env? thing)
         (env-cached-abstract-hash thing))
        ((closure? thing)
         (closure-cached-abstract-hash thing))
        ((binding? thing)
         (binding-cached-abstract-hash thing))
        (else (error "No cache for abstract hash on" thing))))

(define (set-cached-abstract-hash! thing value)
  (cond ((env? thing)
         (set-env-cached-abstract-hash! thing value))
        ((closure? thing)
         (set-closure-cached-abstract-hash! thing value))
        ((binding? thing)
         (set-binding-cached-abstract-hash! thing value))
        (else (error "No cache for abstract hash on" thing))))

(define abstract-hash-cache-table (make-eq-hash-table))
;;; It turns out that I want this hash function to be GC-invariant
;;; (see the commit log) instead of relying on address hashing in
;;; eqv-hash.  While I'm at it, I think I can do a better job of
;;; spreading the love than equal-hash would.
(define abstract-hash
  (memoize-conditionally
   caches-abstract-hash?
   (slot-memoizer cached-abstract-hash set-cached-abstract-hash! #f)
   (memoize-conditionally
    (lambda (thing)
      (or (pair? thing) (symbol? thing) (primitive? thing)))
    abstract-hash-cache-table
    (let ((factor 37)
          (modulus 33554393))
      ;; The factor is a not-too-big prime.  The modulus is the largest
      ;; prime fixnum.  If you can figure out the provenance of the
      ;; remaining magic numbers in this function, I (axch) owe you a
      ;; proverbial beer.  In light of my aversion to alcoholic
      ;; beverages, however, a prize of equal value will have to be
      ;; substituted.
      (define (munch elt total)
        (modulo (+ (* factor total) elt) modulus))
      (lambda (thing)
        (cond ((real? thing)
               ;; EQV-HASH appears, experimentally, to be GC-invariant
               ;; for fixnums and flonums.
               (eqv-hash thing))
              ((null? thing)
               17417)
              ((eq? #t thing)
               17431)
              ((eq? #f thing)
               17443)
              ((abstract-none? thing)
               17471)
              ((abstract-boolean? thing)
               17477)
              ((abstract-real? thing)
               17489)
              ((abstract-gensym? thing)
               (munch (abstract-gensym-min thing)
                      (abstract-gensym-max thing)))
              ;; TODO Do I want to separate abstract and concrete envs
              ;; and closures so that concrete gensyms need not be
              ;; abstract values?
              ((gensym? thing) (* factor (gensym-number thing)))
              ((symbol? thing)
               (string-hash (symbol->string thing)))
              ((primitive? thing)
               (string-hash (symbol->string (primitive-name thing))))
              ((or (pair? thing) (closure? thing) (env? thing)
                   (binding? thing)) ; A binding isn't really an
                                        ; abstract value, but it contains
                                        ; such, and I want to be able to
                                        ; unique them with an abstract
                                        ; table.
               (object-reduce
                (lambda (lst)
                  (reduce munch
                          0
                          (cons (cond ((pair? thing)
                                       17509)
                                      ((closure? thing)
                                       17519)
                                      ((env? thing)
                                       17569)
                                      ((binding? thing)
                                       17579))
                                (map abstract-hash lst))))
                thing))
              (else (internal-error "Do not know how to hash" thing))))))))

(define (abstract-hash-mod thing modulus)
  (modulo (abstract-hash thing) modulus))

(define abstract-hash-table-type
  (make-ontology abstract-hash-mod abstract-equal? #f
                 (and hash-table-types-available?
                      hash-table-entry-type:strong)))

(define make-abstract-hash-table
  (strong-hash-table/constructor abstract-hash-mod abstract-equal? #f))

(define canonical-abstract-values (make-abstract-hash-table))
(define make-abstract-gensym (canonize canonical-abstract-values make-abstract-gensym))
(define %make-env (canonize canonical-abstract-values %make-env))
(define %make-closure (canonize canonical-abstract-values %make-closure))
;(define make-world (canonize canonical-abstract-values make-world))
;(define %make-binding (canonize canonical-abstract-values %make-binding))
(define (reset-canonical-abstract-values!)
  (hash-table/clear! canonical-abstract-values))

;;; Union of shapes --- can be either this or that.  ABSTRACT-UNION is
;;; used on the return values of IF statements and to merge the
;;; results of successive refinements of the same binding.  If we
;;; think of "gensym" shapes as of sets of gensym numbers, then the
;;; union of two "gensym" shapes is given by the union of the
;;; underlying sets.  Because we approximate a set of gensym numbers
;;; by the pair of its minimal and maximal elements, the union of sets
;;; turns into the minimum of the minima paired with the maximum of
;;; the maxima.

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
            (dvl-error "This program is not union-free:" thing1 thing2))))))
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
        ((primitive? thing) '(vector)) ; Primitives have empty closure records
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
  (cond ((some-real? thing) 'real)
        ((some-boolean? thing) 'bool)
        ((null? thing) '())
        ((primitive? thing) '(vector)) ; Primitives have empty closure records
        ((abstract-gensym? thing) 'gensym)
        ((pair? thing)
         `(cons ,(shape->type-declaration (car thing))
                ,(shape->type-declaration (cdr thing))))
        ;; Only replace abstractly-solved closures, not other things
        ((solved-abstractly? thing) '(vector))
        ((closure? thing)
         (abstract-closure->scheme-structure-name thing))
        (else (internal-error "shape->type-declaration loses!" thing))))

(define (needs-translation? thing)
  (not (contains-no-closures? thing)))

(define (contains-no-closures? thing)
  (or (some-boolean? thing)
      (some-real? thing)
      (abstract-none? thing)
      (abstract-gensym? thing)
      (null? thing)
      (and (pair? thing)
           (contains-no-closures? (car thing))
           (contains-no-closures? (cdr thing)))))
