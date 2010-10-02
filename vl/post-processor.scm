(declare (usual-integrations))
;;;; Term-rewriting post-processor

;;; This is by no means a general-purpose Scheme code simplifier.  On
;;; the contrary, it is deliberately and heavily specialized to the
;;; task of removing obvious stupidities from the output of the VL
;;; code generator.

;;; Don't worry about the rule-based term-rewriting system that powers
;;; this.  That is its own pile of stuff, good for a few lectures of
;;; Sussman's MIT class Adventures in Advanced Symbolic Programming.
;;; It works, and it's very good for peephole manipulations of
;;; structured expressions (like the output of the VL code generator).

;;; The rules below consist of a pattern to try to match and an
;;; expression to evaluate to compute a replacement for that match
;;; should a match be found.  Patterns match themselves; the construct
;;; (? name) introduces a pattern variable named name; the construct
;;; (? name ,predicate) is a restrcted pattern variable which only
;;; matches things the predicate accepts; the construct (?? name)
;;; introduces a sublist pattern variable.  The replacement expression
;;; is evaluated in an environment where the pattern variables are
;;; bound to the things they matched.  The rules are applied to every
;;; subexpression of the input expression repeatedly until the result
;;; settles down.

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

    (rule (let ()
	    (? body))
	  body)

    (rule (let (((? name ,generated-temporary?) (cons (? a) (? d))))
	    (? body))
	  (replace-free-occurrences name `(cons ,a ,d) body))

    (rule (let (((? name ,generated-temporary?) (? exp ,symbol?)))
	    (? body))
	  (replace-free-occurrences name exp body))

    (rule (let (((? name ,symbol?) (? exp)))
	    (? name))
	  exp)

    (rule (car (cons (? a) (? d))) a)
    (rule (cdr (cons (? a) (? d))) d)
    )))

