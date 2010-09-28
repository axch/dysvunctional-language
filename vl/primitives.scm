(define-structure (primitive (safe-accessors #t))
  name
  arity
  implementation
  abstract-implementation)

(define (uncurry f)
  (lambda (lst)
    (f (car lst) (cadr lst))))

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

(define (unary-numeric-primitive name base)
  (make-primitive
   name
   1
   base
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (if (abstract-real? arg)
	     abstract-real
	     (base arg))))))

(define (binary-numeric-primitive name base)
  (make-primitive
   name
   2
   (lambda (arg)
     (base (car arg) (cdr arg)))
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (let ((first-arg (car arg))
	       (second-arg (cdr arg)))
	   (if (or (abstract-real? first-arg)
		   (abstract-real? second-arg))
	       abstract-real
	       (base first-arg second-arg)))))))

(define (RxR->bool-primitive name base)
  (make-primitive
   name
   2
   (lambda (arg)
     (base (car arg) (cdr arg)))
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (let ((first (car arg))
	       (second (cdr arg)))
	   (if (or (abstract-real? first)
		   (abstract-real? second))
	       abstract-boolean
	       (base first second)))))))

(define (primitive-type-predicate name base)
  (make-primitive
   name
   1
   base
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all ; Not abstract-bool, we're union-free
	 (base arg)))))

(define (real x)
  (if (real? x)
      x
      (error "A non-real object is asserted to be real" x)))

(define (if-procedure p c a)
  (if p (c) (a)))

(define-syntax define-unary-numeric-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (unary-numeric-primitive 'name name)))))

(define-syntax define-binary-numeric-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (binary-numeric-primitive 'name name)))))

(define-syntax define-RxR->bool-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (RxR->bool-primitive 'name name)))))

(define-syntax define-primitive-type-predicate
  (syntax-rules ()
    ((_ name)
     (add-primitive! (primitive-type-predicate 'name name)))))


(define-unary-numeric-primitive abs)
(define-unary-numeric-primitive exp)
(define-unary-numeric-primitive log)
(define-unary-numeric-primitive sin)
(define-unary-numeric-primitive cos)
(define-unary-numeric-primitive tan)
(define-unary-numeric-primitive asin)
(define-unary-numeric-primitive acos)
(define-unary-numeric-primitive sqrt)

(define-binary-numeric-primitive +)
(define-binary-numeric-primitive -)
(define-binary-numeric-primitive *)
(define-binary-numeric-primitive /)
(define-binary-numeric-primitive atan)
(define-binary-numeric-primitive expt)

(define-RxR->bool-primitive  <)
(define-RxR->bool-primitive <=)
(define-RxR->bool-primitive  >)
(define-RxR->bool-primitive >=)
(define-RxR->bool-primitive  =)

;; TODO Do these really have to be primitive?
(define-primitive-type-predicate null?)
(define-primitive-type-predicate pair?)

(add-primitive!
 (make-primitive
  'real
  1
  real
  (lambda (x)
    (cond ((abstract-all? x) abstract-all)
	  ((abstract-real? x) abstract-real)
	  ((number? x) abstract-real)
	  (else (error "Something known not to be a real number is declared real" x))))))

(define primitive-if
  (make-primitive
   'if-procedure
   3
   (lambda (arg)
     (if-procedure (car arg) (cadr arg) (cddr arg)))
   (lambda (shape analysis)
     (if (abstract-all? shape)
	 abstract-all
	 (let ((predicate (car shape)))
	   (if (not (abstract-boolean? predicate))
	       (if predicate
		   (abstract-result-of (cadr shape) analysis)
		   (abstract-result-of (cddr shape) analysis))
	       (abstract-union (abstract-result-of (cadr shape) analysis)
			       (abstract-result-of (cddr shape) analysis))))))))
(add-primitive! primitive-if)

(define (abstract-result-of thunk-shape analysis)
  ;; This operation only exists because of the way I'm doing IF.
  (refine-apply thunk-shape '() analysis))
