(define (symbol-with-prefix? thing prefix)
  (and (symbol? thing)
       (let ((name (symbol->string thing)))
	 (and (> (string-length name) (string-length prefix))
	      (equal? (string-head name (string-length prefix))
		      prefix)))))

(define (record-accessor-name? thing)
  (symbol-with-prefix? thing "closure-"))

(define (generated-temporary? thing)
  (symbol-with-prefix? thing "temp-"))

(define post-process
  (rule-simplifier
   (list

    (rule (define ((?? line) the-formals)
	    (let (((? name ,symbol?) the-formals))
	      (?? body)))
	  `(define (,@line ,name)
	     ,@body))

    (rule (define (? formals)
	    (let ()
	      (?? body)))
	  `(define ,formals
	     ,@body))

    (rule (let (((? name ,generated-temporary?) (cons (? a) (? d))))
	    (? body))
	  (replace-in-tree name `(cons ,a ,d) body))

    (rule (let (((? name ,generated-temporary?) (? exp ,symbol?)))
	    (? body))
	  (replace-in-tree name exp body))

    (rule (car (cons (? a) (? d))) a)
    (rule (cdr (cons (? a) (? d))) d)
    )))

