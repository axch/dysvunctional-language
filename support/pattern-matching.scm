(declare (usual-integrations))

;;;; A Schemely pattern matching facility.

;;; This is an implementation of the idea described in pattern-matching.txt.
;;;
;;; It differs from what is described in that document as follows:
;;;
;;; - Interface: matchers pass the pieces of the object to the win
;;;   continuation, and nothing to the lose continuation.  Since MIT
;;;   Scheme does not do flow analysis, no measures to make it more
;;;   effective are taken.
;;;
;;; - Naming convention: matchers are named after the type name, with
;;;   no typographic markers.  What is called `pair?*' in the text is
;;;   called `pair' here.
;;;
;;; - List matchers, segment variables, and guards are not implemented.
;;;
;;; - As patterns are implemented, with the syntax (pair a d :as foo)
;;;
;;; - Be sure to (declare (integrate-externals "pattern-matching")) in
;;;   any file that uses this, or you will start consing closures like
;;;   mad and probably suffer around 5x-10x slowdown in case* forms.

;;; This matching and destructuring system depends on two main
;;; components: the matcher procedures and the case* macro.  They
;;; conspire to make examples like this (contrived one) work:
;;;
;;;   (case* foo
;;;    ((pair a (pair ad dd)) (+ a ad dd))
;;;    ((pair _ d) d)
;;;    ((null) 3)
;;;
;;; This evaluates foo; if it turns out to be a pair whose cdr is also
;;; a pair, it will (try to) sum the car, cadr, and cddr of foo;
;;; otherwise if it turns out to be pair, will return its cdr;
;;; otherwise if it turns out to be null, will return 3; otherwise
;;; will return an unspecified value.
;;;
;;; The matcher procedures used in the above example are `pair' and
;;; `null'.  A matcher procedure must accept three arguments: the
;;; object to match, a procedure to call if it matches, and a
;;; procedure to call if it does not.  The meaning of matching depends
;;; on the particular matcher procedure; in the case of `pair' that
;;; would be being a pair, and in the case of `null' that would be
;;; being null.  If the object indeed matches, the matcher procedure
;;; must call its second argument with the match data.  What the match
;;; data is is also defined by the particular matcher procedure -- in
;;; the case of `pair', that would be the car and the cdr of the pair,
;;; and in the case of `null', there is no match data (so the second
;;; argument to `null' must accept no arguments).  If the object does
;;; not match, the matcher procedure must call its third argument with
;;; no arguments.
;;;
;;; The provided matcher procedures are:
;;;
;;;   TODO document provided matcher procedures
;;;
;;; You are free and encouraged to write your own.  Defining them with
;;; define-integrable is recommended for performance (as is
;;; appropriate application of integrate-externals in files that use
;;; your custom matcher procedures.
;;;
;;; The case* macro has the following syntax:
;;;
;;; <case*>   = (case* <expr> <clause> ...)
;;; <clause>  = (<pattern> <body-form> ...)
;;;           | (<matcher> => <receiver>)
;;; <pattern> = _
;;;           | <var>
;;;           | (<matcher> <pattern> ...)
;;;           | (<matcher> <pattern> ... :as <var>)
;;;
;;; <matcher>, <receiver>, <expr>, and <body-form> are Scheme
;;; expressions.
;;;
;;; The semantics of a case* form are as follows.  First, the <expr>
;;; is evaluated (once) to produce an object to be matched against.
;;; Then each clause is tried in turn (described below) until one
;;; matches; in which case the value of the case* form is the value
;;; produced by that clause.  If none of the clauses match, the value
;;; of the case* form is unspecified.
;;;
;;; There are two types of clauses: normal pattern clauses and "arrow
;;; clauses", the latter being distinguished by the presence of the
;;; token `=>' in the second position in the clause.  The arrow
;;; clauses are simpler so we describe them first.
;;;
;;; If the clause is an arrow clause, both expressions in the clause
;;; are evaluated.  The first is expected to return a matcher
;;; procedure, as above, and the second is expected to return a
;;; procedure.  The matcher procedure is then called on the object
;;; being matched against, the procedure returned by the receiver
;;; form, and a nullary procedure that, if invoked, will continue
;;; matching later clauses.  The effect is that if the object matches
;;; the matcher, the case* form will reduce to a call to the receiver
;;; with the match data as defined by the matcher; otherwise
;;; evaluation will continue.
;;;
;;; If the clause is a pattern clause, the behavior depends on the
;;; pattern.  If the pattern is a variable, it will automatically
;;; match, and the body of the clause will be evaluated in an
;;; environment where the object is bound to that variable.  The
;;; special variable `_' is treated as an ignore directive, and the
;;; object is not bound in this case.  If the pattern is a list, then
;;; the first element of the pattern is evaluated to produce a matcher
;;; procedure, as above.  This matcher procedure is called on the
;;; object, a procedure constructed from the rest of the pattern
;;; together with the clause body, and a nullary procedure that will
;;; continue by trying the remaining clauses.  If the remaining
;;; elements of the pattern are all variables, then, if the object
;;; matches, the body will be evaluated in an environment where the
;;; match data is bound to those variables (underscores are again
;;; treated as ignore directives).  If any of the elements of the
;;; pattern are nontrivial subpatterns, the corresponding part of the
;;; object will be matched recursively.  If the whole pattern matches,
;;; the body will be evaluated in an environment where all the parts
;;; are bound to the given names; if not, the next clause will be
;;; tried.
;;;
;;; If the second-to-last element of a pattern is the token `:as',
;;; this is an "as-pattern".  The object being matched against this
;;; pattern will be bound to the last element of the pattern (which
;;; must be a variable), and the match will proceed using the pattern
;;; without the `:as' token or that variable.

;;; TODO Compare to comparable facilities in other languages
;;; - destructuring-bind, bind in Common Lisp
;;; - Clojure pattern matching
;;; - ruby case
;;; TODO Implement pattern guards?  View patterns?
;;; TODO Implement known-length list patterns (issue 1)
;;; TODO Document define-algebraic-matcher

(define-syntax case*
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((expr (cadr form))
           (expr-name (generate-uninterned-symbol 'expr-)))
       (define (arrow-form? clause)
         (and (= 3 (length clause))
              (compare (rename '=>) (cadr clause))))
       (define (ignore? thing)
         (compare thing (rename '_)))
       (define-integrable (as-pattern pattern win lose)
         (let loop ((pattern pattern) (skipped '()))
           (cond ((not (pair? pattern)) (lose))
                 ((null? (cdr pattern)) (lose))
                 ((and (null? (cddr pattern))
                       (symbol? (car pattern))
                       (compare (car pattern) (rename ':as)))
                  (win (reverse skipped) (cadr pattern)))
                 (else (loop (cdr pattern) (cons (car pattern) skipped))))))
       (define (parse-clause clause lose-name)
         (define (arrow-clause matcher procedure)
           `(,matcher ,expr-name ,procedure ,lose-name))
         (define (standard-clause expr-name pattern body)
           (define (standard-pattern expr-name pattern body)
             (receive (variables body)
               (let loop ((subpatterns (cdr pattern)))
                 (cond ((null? subpatterns) (values '() body))
                       ((pair? (car subpatterns))
                        (receive (true-subpattern variable)
                          (as-pattern (car subpatterns)
                           values
                           (lambda ()
                             (values (car subpatterns)
                                     (generate-uninterned-symbol 'part-))))
                          (receive (variables body) (loop (cdr subpatterns))
                            (values (cons variable variables)
                                    (list (standard-pattern variable true-subpattern body))))))
                       ;; Assume identifier
                       ((ignore? (car subpatterns))
                        (let ((variable (generate-uninterned-symbol 'dead-)))
                          (receive (variables body) (loop (cdr subpatterns))
                            (values (cons variable variables)
                                    (cons `(declare (ignore ,variable))
                                          body)))))
                       (else ;; Assume identifier
                        (receive (variables body) (loop (cdr subpatterns))
                          (values (cons (car subpatterns) variables)
                                  body)))))
               `(,(car pattern) ,expr-name (,(rename 'lambda) ,variables ,@body) ,lose-name)))
           (cond ((pair? pattern)
                  (as-pattern pattern
                   (lambda (true-pattern variable)
                     `(let ((,variable ,expr-name))
                        ,(standard-pattern expr-name true-pattern body)))
                   (lambda ()
                     (standard-pattern expr-name pattern body))))
                 ((ignore? pattern)
                  `(let ()
                     (declare (ignore ,lose-name))
                     ,@body))
                 (else
                  `(let ((,pattern ,expr-name))
                     (declare (ignore ,lose-name))
                     ,@body))))
         (if (arrow-form? clause)
             (arrow-clause (car clause) (caddr clause))
             (standard-clause expr-name (car clause) (cdr clause))))
       `(,(rename 'let) ((,expr-name ,expr))
          ,(let loop ((clauses (cddr form)))
             (if (null? clauses)
                 (rename 'unspecific)
                 (let ((lose-name (generate-uninterned-symbol 'lose-)))
                   `(,(rename 'let) ((,lose-name (,(rename 'lambda) () ,(loop (cdr clauses)))))
                     ;; This integration may not be appropriate if the
                     ;; body refers to the lose-name more than once...
                     (declare (integrate-operator ,lose-name))
                     ,(parse-clause (car clauses) lose-name))))))))))

(define-syntax define-algebraic-matcher
  (syntax-rules ()
    ((_ matcher predicate accessor ...)
     (define-integrable (matcher thing win lose)
       (if (predicate thing)
           (win (accessor thing) ...)
           (lose))))))

(define-integrable (id-project x) x)
(define-algebraic-matcher pair pair? car cdr)
(define-algebraic-matcher null null?)
(define-algebraic-matcher boolean boolean? id-project)
(define-algebraic-matcher number number? id-project)

(define-syntax lambda-case*
  ;; This is not a syntax-rules macro because case* will make some of
  ;; its subforms into names that are bound in other subforms, and
  ;; I fear that syntax-rules might interfere with this.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((clauses (cdr form))
           (bound-name (generate-uninterned-symbol)))
       `(,(rename 'lambda) (,bound-name)
         (,(rename 'case*) ,bound-name
           ,@clauses))))))

(define-syntax define-case*
  ;; This is not a syntax-rules macro because case* will make some of
  ;; its subforms into names that are bound in other subforms, and
  ;; I fear that syntax-rules might interfere with this.
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((name (cadr form))
           (clauses (cddr form))
           (defined-name (generate-uninterned-symbol)))
       `(,(rename 'define) (,name ,defined-name)
         (,(rename 'case*) ,defined-name
           ,@clauses))))))

;; TODO good error messages if syntax is wrong; define all needed matchers
