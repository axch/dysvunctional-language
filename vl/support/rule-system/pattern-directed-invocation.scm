(declare (usual-integrations))

;;; A rule, in this terminology, is a pattern and a handler.  The
;;; pattern determines the applicability of the rule and the match
;;; bindings that enable it, and the handler can compute an arbitrary
;;; value from them.  Once constructed, a rule is a procedure that
;;; accepts a datum, and returns either #f if the pattern doesn't
;;; match or the value of the handler when applied to the dictionary
;;; if it does.  This code contains a solution to the consequent
;;; sentinel value issue, but I don't like it.

(define (make-rule pattern handler)
  (if (user-handler? handler)
      (make-rule pattern (user-handler->system-handler
			  handler (match:pattern-names pattern)))
      (let ((pattern-combinator (->combinators pattern)))
	(lambda (data #!optional succeed fail)
	  (if (default-object? succeed)
	      (set! succeed (lambda (value fail) value)))
	  (if (default-object? fail)
	      (set! fail (lambda () #f)))
	  (pattern-combinator data
	   (lambda (dict fail)
	     (handler dict succeed fail))
	   fail)))))

;;; A pattern directed operator is a collection of rules, one of which
;;; is expected to match any datum that the operator may be given.
;;; The operator tries the rules in order until the first matches, and
;;; returns the value given by that one; if none match, it errors out.

(define (make-pattern-operator #!optional rules)
  (define (operator self . arguments)
    (define (succeed value fail) value)
    (define (fail)
      (error "No applicable operations" self arguments))
    (try-rules arguments (entity-extra self) succeed fail))
  (make-entity operator (if (default-object? rules) '() rules)))

(define (try-rules data rules succeed fail)
  (let per-rule ((rules rules))
    (if (null? rules)
	(fail)
	((car rules) data succeed
	 (lambda ()
	   (per-rule (cdr rules)))))))

(define (attach-rule! operator rule)
  (set-entity-extra! operator
   (cons rule (entity-extra operator))))

;;; The user-handler is expected to be a procedure that binds the
;;; variables that appear in the match and uses them somehow.  This
;;; converts it into a combinator that accepts the match dictionary,
;;; and success and failure continuations.  Does not deal with
;;; optional and rest arguments in the handler.

(define (user-handler->system-handler user-handler #!optional default-argl)
  (let ((handler-argl (procedure-argl user-handler default-argl)))
    (system-handler!
     (lambda (dict succeed fail)
       (define (matched-value name)
	 (match:value
	  (or (match:lookup name dict)
	      (error "Handler asked for unknown name"
		     name dict))))
       (let* ((argument-list (map matched-value handler-argl))
	      (user-answer (apply user-handler argument-list)))
	 (if user-answer
	     (succeed user-answer fail)
	     (fail)))))))

(define (user-handler? thing)
  (not (system-handler? thing)))

(define (system-handler? thing)
  (eq-get thing 'system-handler))

(define (system-handler! thing)
  (eq-put! thing 'system-handler #t)
  thing)

(define (->combinators pattern)
  (let ((class-combinator
	 (match:->combinators pattern)))
    (lambda (data succeed fail)
      (or (class-combinator data '()
	   (lambda (value)
	     (succeed value (lambda () #f))))
	  (fail)))))

;;; The RULE macro is convenient syntax for writing rules.  A rule is
;;; written as a quoted pattern and an expression.  If the pattern
;;; matches, the expression will be evaluated in an environment that
;;; includes the bindings of the pattern variables.  If the expression
;;; returns #f, that will cause the pattern matcher to backtrack.
(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((pattern (cadr form))
	   (handler-body (caddr form)))
       `(make-rule 
	 ,(close-syntax pattern use-env)
	 ,(compile-handler handler-body use-env
			   (match:pattern-names pattern)))))))

(define (compile-handler form env names)
  ;; See magic in utils.scm
  (make-lambda names env
    (lambda (env*) (close-syntax form env*))))

#|
 (pp (syntax '(rule '(* (? a) (? b))
		    (and (expr<? a b)
                         `(* ,a ,b)))
             (the-environment)))

; (make-rule '(* (? a) (? b))
;  (lambda (b a)
;    (and (expr<? a b)
;         (list '* a b))))
;Unspecified return value
|#

;;; Pattern-directed factorial, with and without the rule macro.

#|
 (define factorial (make-pattern-operator))

 (attach-rule! factorial (rule '(0) 1))

 (attach-rule! factorial
  (rule `((? n ,positive?))
        (* n (factorial (- n 1)))))

 (factorial 10)
 ;Value: 3628800
|#

#|
 (define factorial (make-pattern-operator))

 (attach-rule! factorial
  (make-rule '((? n))
   (lambda (n) (* n (factorial (- n 1))))))

 (attach-rule! factorial
  (make-rule '(0) (lambda () 1)))

 (factorial 10)
 ;Value: 3628800
|#

