(declare (usual-integrations))
;;;; VL primitive procedures

;;; A VL primitive procedure needs to tell the concrete evaluator how
;;; to execute it, the analyzer how to think about calls to it, and
;;; the code generator how to emit calls to it.  These are the
;;; implementation, abstract-implementation, and name and arity slots,
;;; respectively.

(define-structure (primitive (safe-accessors #t))
  name
  arity
  implementation
  abstract-implementation)

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

;;; Most primitives fall into a few natural classes:

;;; Unary numeric primitives just have to handle getting abstract
;;; values for arguments (either ABSTRACT-ALL or ABSTRACT-REAL).
(define (unary-numeric-primitive name base)
  (make-primitive name 1
   base
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all
	 (if (abstract-real? arg)
	     abstract-real
	     (base arg))))))

;;; Binary numeric primitives also have to destructure their input,
;;; because the VL system will hand it in as a pair.
(define (binary-numeric-primitive name base)
  (make-primitive name 2
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

;;; Primitive type testers (namely NULL? and PAIR?) need to take care
;;; not to offer information too soon.  In principle, of course, no
;;; matter what argument is passed to NULL? or PAIR?, the answer is
;;; guaranteed to be some boolean.  However, IF treats an
;;; ABSTRACT-BOOLEAN predicate as a promise that the analysis has
;;; figured out that either branch is definitely capable of being
;;; taken.  Therefore, primitive type testers must wait until the
;;; analysis has determined the shape their argument will take, and
;;; compute the answer only then.

(define (primitive-type-predicate name base)
  (make-primitive name 1
   base
   (lambda (arg)
     (if (abstract-all? arg)
	 abstract-all ; Not abstract-bool, we're union-free
	 (base arg)))))

;;; Binary numeric comparisons have all the concerns of binary numeric
;;; procedures and of unary type testers.
(define (RxR->bool-primitive name base)
  (make-primitive name 2
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

(define-primitive-type-predicate null?)
(define-primitive-type-predicate pair?)

;;; The primitives REAL and IF-PROCEDURE are very special.

(define (real x)
  (if (real? x)
      x
      (error "A non-real object is asserted to be real" x)))

;;; REAL must take care to always emit an ABSTRACT-REAL during
;;; analysis, even though it's the identity function at runtime.
;;; Without this, "union-free flow analysis" would amount to running
;;; the program very slowly at analysis time until the final answer
;;; was computed.

(add-primitive!
 (make-primitive 'real 1
  real
  (lambda (x)
    (cond ((abstract-all? x) abstract-all)
	  ((abstract-real? x) abstract-real)
	  ((number? x) abstract-real)
	  (else (error "A known non-real is declared real" x))))))

(define (if-procedure p c a)
  (if p (c) (a)))

;;; IF is even more special than REAL.  This definition does not
;;; capture how special IF really is, because the analyzer and the
;;; code generator check explicitly for whether they've hit an IF or
;;; not.  I am open to suggestions about how to improve this.

(define primitive-if
  (make-primitive 'if-procedure 3
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
	       (abstract-union
		(abstract-result-of (cadr shape) analysis)
		(abstract-result-of (cddr shape) analysis))))))))
(add-primitive! primitive-if)

(define (abstract-result-of thunk-shape analysis)
  ;; N.B. ABSTRACT-RESULT-OF only exists because of the way I'm doing IF.
  (refine-apply thunk-shape '() analysis))
