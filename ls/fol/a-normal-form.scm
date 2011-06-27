(declare (usual-integrations))
;;;; (Approximate) A-normal form conversion

;;; In order to do a good job of scalar replacement of aggregates, the
;;; program being replaced needs to have names for all interesting
;;; intermediate values (so that those names can serve as a place to
;;; hang information about their pre-sra types and their post-sra
;;; replacement names).  A-normal form was designed to serve exactly
;;; this purpose; but I don't need a full A-normal form to achieve my
;;; aim.  To that end, this program converts a FOL program into
;;; "approximate" A-normal form.

;;; Approximate A-normal form requires that all procedure applications
;;; and all mutlivalue returns apply to (resp. return) variables or
;;; constants, rather than the results of any compound computations.
;;; This differs from full A-normal form in that, for example, the
;;; subexpressions of IF do not have to be variables, and parallel LET
;;; is still allowed.

;;; The precise grammar of FOL in approximate ANF is the same as the
;;; normal FOL grammar, except for replacing the <expression>,
;;; <access>, and <construction> nonterminals with the following:
;;;
;;; expression = <simple-expression>
;;;            | (if <expression> <expression> <expression>)
;;;            | (let ((<data-var> <expression>) ...) <expression>)
;;;            | (let-values (((<data-var> <data-var> <data-var> ...) <expression>))
;;;                <expression>)
;;;            | <access>
;;;            | <construction>
;;;            | (values <simple-expression> <simple-expression> <simple-expression> ...)
;;;            | (<proc-var> <simple-expression> ...)
;;;
;;; simple-expression = <data-var> | <number> | <boolean> | ()
;;;
;;; access = (car <simple-expression>)
;;;        | (cdr <simple-expression>)
;;;        | (vector-ref <simple-expression> <integer>)
;;;
;;; construction = (cons <simple-expression> <simple-expression>)
;;;              | (vector <simple-expression> ...)

;;; The following program converts an arbitrary FOL expression into
;;; approximate ANF.  The way to do this is to recur down the
;;; structure of the FOL expression and, should a general <expression>
;;; ever be found in any place where there should only be a
;;; <simple-expression>, introduce a fresh variable binding to hold
;;; the result of that <expression> and put this variable in that
;;; place.  Such a variable will never need to capture a multivalue
;;; return, because the places where <simple-expression>s are needed
;;; only accept single values by the rules of FOL anyway.

(define (approximate-anf expr)
  (define (loop expr)
    (cond ((simple-form? expr) expr)
          ((if-form? expr)
           `(if ,(loop (cadr expr))
                ,(loop (caddr expr))
                ,(loop (cadddr expr))))
          ((lambda-form? expr)
           (approximate-anf-lambda expr))
          ((let-form? expr)
           (approximate-anf-let expr))
          ((let-values-form? expr)
           (approximate-anf-let-values expr))
          ((begin-form? expr)
           (map loop expr))
          ((definition? expr)
           (approximate-anf-definition expr))
          (else ; access, construction, application, or multiple value return
           (approximate-anf-application expr))))
  (define (approximate-anf-lambda expr)
    `(lambda ,(cadr expr)
       ,(loop (caddr expr))))
  (define (approximate-anf-let expr)
    `(let ,(map (lambda (binding)
                  `(,(car binding) ,(loop (cadr binding))))
                (cadr expr))
       ,(loop (caddr expr))))
  (define approximate-anf-let-values
    (rule `(let-values (((? names) (? exp)))
             (? body))
          `(let-values ((,names ,(loop exp)))
             ,(loop body))))
  (define approximate-anf-definition
    (rule `(define (? formals)
             (argument-types (?? stuff))
             (? body))
          `(define ,formals
             (argument-types ,@stuff)
             ,(loop body))))
  (define (approximate-anf-application expr)
    (rename-nontrivial-expressions
     expr
     (lambda (results names)
       (if (not (null? names))
           (loop `(let ,names ,results))
           expr))))
  (define (rename-nontrivial-expressions exprs win)
    (if (null? exprs)
        (win '() '())
        (rename-nontrivial-expression
         (car exprs)
         (lambda (result names)
           (rename-nontrivial-expressions (cdr exprs)
            (lambda (results more-names)
              (win (cons result results)
                   (append names more-names))))))))
  (define (rename-nontrivial-expression expr win)
    (if (simple-form? expr)
        (win expr '())
        (let ((name (make-name 'anf)))
          (win name `((,name ,expr))))))
  (loop expr))

;;; Checking whether a form is already in approximate ANF amounts to
;;; checking whether the anf converter will do anything to it.
(define (approximate-anf? expr)
  (equal? expr (approximate-anf expr)))

;;;; A note on chained access

;;; An access chain like
;;; (car (cdr (car ...)))
;;; or a construction chain like
;;; (cons (vector (cons ... ...) ...) ...)
;;; needs to become, after SRA, a single transfer through a multiple
;;; value bind and multiple value return.  Introducing intermediate
;;; names at the ANF stage for all the intermediate values in such a
;;; chain has the effect that SRA will turn that chain into a sequence
;;; of multiple value binds and returns, and leave it to alias
;;; elimination to collapse the sequence into one.  In principle, one
;;; could write a cleverer ANF transformer that goes to a looser
;;; approximation of ANF that allows access and construction sequences
;;; without naming the intermediates; and a cleverer SRA that will
;;; transform such sequences directly into one multiple value bind and
;;; return; and thereby avoid creating extra work for the alias
;;; eliminator.  I have not chosen to do so; partially because the
;;; alias eliminator would be needed anyway.

;;; On the other hand, leaving constructor chains in place would have
;;; the advantage that the reconstruction of structured shapes
;;; expected by the outside world (see sra.scm) would not break that
;;; more lenient approximate ANF.

;;;; Reverse A-normal form conversion

;;; When we want to read code (as opposed to compile it), all the
;;; extra names introduced by A-normal form just get in the way.  The
;;; following little snippet inlines the bindings of variable names
;;; that are used only once.

;;; This is safe assuming the program has been alpha renamed
;;; Otherwise it would break because of inlining y in
;;; (let ((x 1))
;;;   (let ((y (+ x 1)))
;;;     (let ((x 3))
;;;       (+ x y))))

(define reverse-anf
  (rule-simplifier
   (list
    (rule `(let ((?? bindings1)
                 ((? name ,fol-var?) (? exp))
                 (?? bindings2))
             (?? body))
          (and (unique-in-tree? name body)
               (tidy-empty-let
                `(let (,@bindings1
                       ,@bindings2)
                   ,@(replace-in-tree name exp body))))))))

(define (unique-in-tree? thing tree)
  (define (walk tree count)
    (cond ((equal? thing tree)
           (+ count 1))
          ((pair? tree)
           (let ((left-count (walk (car tree) count)))
             (if (<= left-count 1)
                 (walk (cdr tree) left-count)
                 left-count)))
          (else (+ count 0))))
  (= 1 (walk tree 0)))
