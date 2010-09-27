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

(add-primitive! (binary-numeric-primitive '+ +))
(add-primitive! (binary-numeric-primitive '- -))
(add-primitive! (binary-numeric-primitive '* *))
(add-primitive! (binary-numeric-primitive '/ /))
(add-primitive! (RxR->bool-primitive '<  <))
(add-primitive! (RxR->bool-primitive '<= <=))
(add-primitive! (RxR->bool-primitive '>  >))
(add-primitive! (RxR->bool-primitive '>= >=))
(add-primitive! (RxR->bool-primitive '=  =))
;; TODO Do these really have to be primitive?
(add-primitive! (primitive-type-predicate 'null? null?))
(add-primitive! (primitive-type-predicate 'pair? pair?))
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
