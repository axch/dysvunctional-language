(define *peephole-optimizer-available?* #f)

(load-relative "../../rule-system/load-for-use.scm")

(define (safe-to-replicate? exp)
  (cond ((symbol? exp)
	 ;; This should just be #t, but doing it this way is a hack to
	 ;; get the peephole optimizer to expand things the way I
	 ;; want.
	 (not (eq? 'the-formals exp)))
	((and (pair? exp) (eq? (car exp) 'cons))
	 (and (safe-to-replicate? (cadr exp))
	      (safe-to-replicate? (caddr exp))))
	(else #f)))

(define peephole-optimizer
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

    (rule (let (((? name ,symbol?) (? exp ,safe-to-replicate?)))
	    (? body))
	  (replace-in-tree name exp body))

    (rule (car (cons (? a) (? d))) a)
    (rule (cdr (cons (? a) (? d))) d)
    )))

