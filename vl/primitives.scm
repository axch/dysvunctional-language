(declare (usual-integrations))
;;;; VL primitive procedures

;;; A VL primitive procedure needs to tell the concrete evaluator how
;;; to execute it, the analyzer how to think about calls to it, and
;;; the code generator how to emit calls to it.  These are the
;;; implementation, abstract-implementation and expand-implementation,
;;; and name and arity slots, respectively.

(define-structure (primitive (safe-accessors #t))
  name
  arity
  implementation
  abstract-implementation
  expand-implementation)

(define *primitives* '())

(define (add-primitive! primitive)
  (set! *primitives* (cons primitive *primitives*)))

;;; Most primitives fall into a few natural classes:

;;; Unary numeric primitives just have to handle getting abstract
;;; values for arguments (to wit, ABSTRACT-REAL).
(define (unary-numeric-primitive name base)
  (make-primitive name 1
   base
   (lambda (arg analysis)
     (if (abstract-real? arg)
	 abstract-real
	 (base arg)))
   (lambda (arg analysis) '())))

;;; Binary numeric primitives also have to destructure their input,
;;; because the VL system will hand it in as a pair.
(define (binary-numeric-primitive name base)
  (make-primitive name 2
   (lambda (arg)
     (base (car arg) (cdr arg)))
   (lambda (arg analysis)
     (let ((first-arg (car arg))
	   (second-arg (cdr arg)))
       (if (or (abstract-real? first-arg)
	       (abstract-real? second-arg))
	   abstract-real
	   (base first-arg second-arg))))
   (lambda (arg analysis) '())))

(define (primitive-type-predicate name base)
  (make-primitive name 1
   base
   (lambda (arg analysis)
     (base arg))
   (lambda (arg analysis) '())))

(define (R->bool-primitive name base)
  (make-primitive name 1
   base
   (lambda (arg analysis)
     (if (abstract-real? arg)
	 (if (not (eq? base real?)) ;; Grr!
	     abstract-boolean
	     #t)
	 (base arg)))
   (lambda (arg analysis) '())))

;;; Binary numeric comparisons have all the concerns of binary numeric
;;; procedures and of unary type testers.
(define (RxR->bool-primitive name base)
  (make-primitive name 2
   (lambda (arg)
     (base (car arg) (cdr arg)))
   (lambda (arg analysis)
     (let ((first (car arg))
	   (second (cdr arg)))
       (if (or (abstract-real? first)
	       (abstract-real? second))
	   abstract-boolean
	   (base first second))))
   (lambda (arg analysis) '())))

(define-syntax define-unary-numeric-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (unary-numeric-primitive 'name name)))))

(define-syntax define-binary-numeric-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (binary-numeric-primitive 'name name)))))

(define-syntax define-primitive-type-predicate
  (syntax-rules ()
    ((_ name)
     (add-primitive! (primitive-type-predicate 'name name)))))

(define-syntax define-R->bool-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (R->bool-primitive 'name name)))))

(define-syntax define-RxR->bool-primitive
  (syntax-rules ()
    ((_ name)
     (add-primitive! (RxR->bool-primitive 'name name)))))

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

(define-primitive-type-predicate null?)
(define-primitive-type-predicate pair?)

(define (vl-procedure? thing)
  (or (primitive? thing)
      (closure? thing)))
(add-primitive! (primitive-type-predicate 'procedure? vl-procedure?))

(define-RxR->bool-primitive  <)
(define-RxR->bool-primitive <=)
(define-RxR->bool-primitive  >)
(define-RxR->bool-primitive >=)
(define-RxR->bool-primitive  =)

(define-R->bool-primitive real?)
(define-R->bool-primitive zero?)
(define-R->bool-primitive positive?)
(define-R->bool-primitive negative?)

;;; The primitive REAL is special.

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
  (lambda (x analysis)
    (cond ((abstract-real? x) abstract-real)
	  ((number? x) abstract-real)
	  (else (error "A known non-real is declared real" x))))
  (lambda (arg analysis) '())))

;;; READ-REAL

(define read-real read)
(add-primitive!
 (make-primitive 'read-real 0
  read-real
  (lambda (x analysis)
    abstract-real)
  (lambda (arg analysis) '())))

(define (write-real x)
  (write x)
  (newline)
  x)
(add-primitive!
 (make-primitive 'write-real 1
  write-real
  (lambda (x analysis) x)
  (lambda (arg analysis) '())))

;;; IF-PROCEDURE is even more special than REAL, because it is the
;;; only primitive that accepts VL closures as arguments and invokes
;;; them internally.  That is handled transparently by the concrete
;;; evaluator, but IF-PROCEDURE must be careful to analyze its own
;;; return value as being dependent on the return values of its
;;; argument closures, and let the analysis know which of its closures
;;; it will invoke and with what arguments as the analysis discovers
;;; knowledge about IF-PROCEDURE's predicate argument.  Also, the code
;;; generator detects and special-cases IF-PROCEDURE because it wants
;;; to emit native Scheme IF statements in correspondence with VL IF
;;; statements.

(define (if-procedure p c a)
  (if p (c) (a)))

(define primitive-if
  (make-primitive 'if-procedure 3
   (lambda (arg)
     (if-procedure (car arg) (cadr arg) (cddr arg)))
   (lambda (shape analysis)
     (let ((predicate (car shape)))
       (if (not (abstract-boolean? predicate))
	   (if predicate
	       (abstract-result-of (cadr shape) analysis)
	       (abstract-result-of (cddr shape) analysis))
	   (abstract-union
	    (abstract-result-of (cadr shape) analysis)
	    (abstract-result-of (cddr shape) analysis)))))
   (lambda (arg analysis)
     (let ((predicate (car arg))
	   (consequent (cadr arg))
	   (alternate (cddr arg)))
       (define (expand-thunk-application thunk)
	 (analysis-expand
	  `(,(closure-expression thunk) ())
	  (closure-env thunk)
	  analysis))
       (if (not (abstract-boolean? predicate))
	   (if predicate
	       (expand-thunk-application consequent)
	       (expand-thunk-application alternate))
	   (lset-union same-analysis-binding?
		       (expand-thunk-application consequent)
		       (expand-thunk-application alternate)))))))
(add-primitive! primitive-if)

(define (abstract-result-of thunk-shape analysis)
  ;; N.B. ABSTRACT-RESULT-OF only exists because of the way I'm doing IF.
  (refine-apply thunk-shape '() analysis))
