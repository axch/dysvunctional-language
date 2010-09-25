;;; The computed analysis provides a complete description of what's
;;; going on in a program.  The code generator produces code in the
;;; target language (in this case, MIT Scheme) that's structurally
;;; isomorphic to the original, specialized VL code.  The idea is to
;;; create an MIT Scheme procedure definition for every pair of VL
;;; closure and abstract value it gets applied to (if the result is a
;;; non-void abstract value); an MIT Scheme structure definition for
;;; every compound structure (including converted closures) that
;;; occurs; and an MIT Scheme expression for the entry point.  This
;;; pile should have the property that all function calls are to known
;;; targets.

(define *symbol-count* 0)

(define (make-name prefix)
  (set! *symbol-count* (+ *symbol-count* 1))
  (symbol prefix *symbol-count*))

(define (vl-variable->scheme-variable var)
  var)

(define (vl-variable->scheme-field-name var)
  var)

(define (vl-variable->scheme-record-access var)
  `(record-get the-closure ',(vl-variable->scheme-field-name var)))

;; TODO This is really part of the "runtime system".
;; TODO I can get rid of the awful record access mechanism for getting
;; at closure-converted variables by passing an extra bit of data
;; through the code generator, which is some ID for the type of the
;; closure record that obtains in this place.
(define (record-get record field-name)
  ((record-accessor (record-type-descriptor record)
		    field-name)
   record))

;; TODO Should this really be an eq? hash table, or should I make an
;; abstract-equal? hash table for these?
(define *closure-names* (make-eq-hash-table))

(define (abstract-closure->scheme-structure-name closure)
  (hash-table/lookup *closure-names* closure
   (lambda (value)
     value)
   (lambda ()
     (let ((answer (make-name 'closure-)))
       (hash-table/put! *closure-names* closure answer)
       answer))))

(define (abstract-closure->scheme-constructor-name closure)
  (symbol 'make- (abstract-closure->scheme-structure-name closure)))

;; TODO Can I really get away with an equal? hash table here?
(define *call-site-names* (make-equal-hash-table))

(define (call-site->scheme-function-name closure abstract-arg)
  (hash-table/lookup *call-site-names* (cons closure abstract-arg)
   (lambda (value)
     value)
   (lambda ()
     (let ((answer (make-name 'operation-)))
       (hash-table/put! *call-site-names* (cons closure abstract-arg) answer)
       answer))))

(define (compile exp abstract-env analysis)
  (cond ((constant? exp) exp)
	((null? exp) ''())
	((variable? exp)
	 (abstract-lookup exp abstract-env
	  (lambda (v) (vl-variable->scheme-variable exp))
          (lambda ()
	    (vl-variable->scheme-record-access exp))))
	((pair? exp)
	 (cond ((eq? (car exp) 'lambda)
		(cons (abstract-closure->scheme-constructor-name
		       (refine-eval-once exp abstract-env analysis))
		      (map (lambda (var)
			     (compile var abstract-env analysis))
			   (free-variables exp))))
	       ((eq? (car exp) 'cons)
		`(cons ,(compile (cadr exp) abstract-env analysis)
		       ,(compile (caddr exp) abstract-env analysis)))
	       (else
		(compile-apply exp abstract-env analysis))))
	(else
	 (error "Invaid expression in code generation"
		exp abstract-env analysis))))

(define (compile-apply exp abstract-env analysis)
  (let ((operator (refine-eval-once (car exp) abstract-env analysis))
	(operands (refine-eval-once (cdr exp) abstract-env analysis)))
    (cond ((primitive? operator)
	   `(,(primitive-name operator)
	     ,(compile (cadr exp) abstract-env analysis)))
	  ((closure? operator)
	   `(,(call-site->scheme-function-name operator operands)
	     ,(compile (car exp) abstract-env analysis)
	     ,(compile (cadr exp) abstract-env analysis)))
	  (else
	   (error "Invalid operator in code generation"
		  exp abstract-env analysis)))))
