(declare (usual-integrations))

;;; A rule, in this terminology, is a pattern and a handler.  The
;;; pattern determines the applicability of the rule and the match
;;; bindings that enable it, and the handler can compute an arbitrary
;;; value from them.  Once constructed, a rule is a procedure that
;;; accepts a datum, and returns either #f if the pattern doesn't
;;; match or the value of the handler when applied to the dictionary
;;; if it does.  This code does not solve the consequent sentinel
;;; value issue.

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

(define (try-rules data rules succeed fail)
  (let per-rule ((rules rules))
    (if (null? rules)
	(fail)
	((car rules) data succeed
	 (lambda ()
	   (per-rule (cdr rules)))))))

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
	 (dict:value
	  (or (dict:lookup name dict)
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
