(declare (usual-integrations))
;;;; Evaluator

;;; SLAD is a simple eval-apply interpreter in the fine tradition of
;;; (COND ((EQ? (CAR MUMBLE) (QUOTE FOO)).  All the interesting action
;;; is in the AD handling: forward-mode.scm, and primitives.scm.

(define (slad-eval form env)
  (cond ((constant? form)
	 (constant-value form))
	((variable? form)
	 (lookup form env))
	((pair-form? form)
	 (make-slad-pair (slad-eval (car-subform form) env)
			 (slad-eval (cdr-subform form) env)))
	((lambda-form? form)
	 (make-closure form env))
	((application? form)
	 (slad-apply (slad-eval (operator-subform form) env)
		     (slad-eval (operand-subform form) env)))
	(else
	 (error "Invalid expression type" form env))))

(define (slad-apply proc arg)
  (cond ((closure? proc)
	 (slad-eval (closure-body proc)
		    (extend-env (closure-formal proc)
				arg
				(closure-env proc))))
	((primitive? proc)
	 ((primitive-implementation proc) arg))
	(else
	 (error "Invalid procedure type" proc arg))))

;;;; Entry points

;;; SLAD-DO evaluates a single SLAD form in a fresh initial environment.
(define (slad-do form)
  (slad-eval (macroexpand (slad-prepare form)) (initial-slad-user-env)))

;;; SLAD-EVAL-FILE reads a file of SLAD source and evaluates it in a
;;; fresh initial environment.
(define (slad-eval-file filename)
  (let ((forms (read-source filename)))
    (write (slad-do `(let () ,@forms)))
    (newline)))

(define my-pathname (self-relatively working-directory-pathname))
(define stdlib (string-append (->namestring my-pathname) "stdlib.slad"))

(define (slad-prepare form)
  `(let ()
     ,@(read-source stdlib)
     ,form))
