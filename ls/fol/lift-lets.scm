(declare (usual-integrations))
(declare (integrate-external "syntax"))

;;; The purpose of lifting lets is to increase the scopes of variables
;;; without affecting when and whether their values are computed.
;;; This is useful for common subexpression elimination, because
;;; previously computed expressions remain available longer.

;;; The action of lifting lets is to find LET expressions that occur
;;; in strict contexts and to lift the LET binding outside the
;;; context, but leave the LET body in place.  For example, since the
;;; binding of an outer LET is a strict context, we get
;;;
;;; (let ((x (let ((y 4)) y)))
;;;   x)
;;; ===>
;;; (let ((y 4))
;;;   (let ((x y))
;;;     x))

;;; Lifting lets is safe assuming the program has unique bound names;
;;; if not, it can break because of
;;;
;;; (let ((x 3))
;;;   (let ((y (let ((x 4)) x)))
;;;     x))
;;;
;;; Unfortunately, mere lack of shadowing is not enough, as lifting
;;; lets can introduce shadowing because of
;;;
;;; (let ((x (let ((y 3)) y)))
;;;   (let ((y 4))
;;;     y))

;;; The grammar of a FOL program whose LETs have been lifted is the
;;; same as the normal FOL grammar, except for replacing the
;;; <expression>, <access>, and <construction> nonterminals with the
;;; following:
;;;
;;; expression = <non-let>
;;;            | (let ((<data-var> <non-let>) ...) <expression>)
;;;            | (let-values (((<data-var> <data-var> <data-var> ...) <non-let>))
;;;                <expression>)
;;;
;;; non-let = <data-var> | <number> | <boolean> | ()
;;;         | (if <non-let> <expression> <expression>)
;;;         | (lambda (<data-var>) <expression>)  ; for escape only
;;;         | <access>
;;;         | <construction>
;;;         | (values <non-let> <non-let> <non-let> ...)
;;;         | (<proc-var> <non-let> ...)
;;;
;;; access = (car <non-let>)
;;;        | (cdr <non-let>)
;;;        | (vector-ref <non-let> <integer>)
;;;
;;; construction = (cons <non-let> <non-let>)
;;;              | (vector <non-let> ...)

;;; TODO Describe the algorithm.

(define (%lift-lets program)
  (if (begin-form? program)
      `(begin
         ,@(map lift-lets-definition (except-last-pair (cdr program)))
         ,(lift-lets-expression (last program)))
      (lift-lets-expression program)))

(define lift-lets-definition
  (rule `(define (? formals)
           (argument-types (?? stuff))
           (? body))
        `(define ,formals
           (argument-types ,@stuff)
           ,(lift-lets-expression body))))

(define (lift-lets-expression expr)
  ;; This is written in continuation passing style because the
  ;; recursive call returns two things: the rewritten expression, and
  ;; the list of bindings that this expression seeks to introduce.
  ;; The bindings lists are represented as functions that will wrap a
  ;; given expression in that binding list, for fast appending.
  (define null (lambda (expr) expr))
  (define (singleton var exp)
    (lambda (expr)
      `(let ((,var ,exp))
         ,expr)))
  (define (values-singleton names exp)
    (lambda (expr)
      `(let-values ((,names ,exp))
         ,expr)))
  (define (append lst1 lst2)
    (lambda (expr)
      (lst1 (lst2 expr))))
  (define (build expr lst)
    (lst expr))
  (define (loop expr)
    (case* expr
      ((simple-form _) (values expr null))
      (if-form => lift-lets-from-if)
      (let-form => lift-lets-from-let)
      (let-values-form => lift-lets-from-let-values)
      (lambda-form => lift-lets-from-lambda)
      (_ ;; general application
       (lift-lets-from-application expr))))
  (define (lift-lets-from-if predicate consequent alternate)
    (receive (new-pred pred-binds) (loop predicate)
      (values `(if ,new-pred
                   ,(lift-lets-expression consequent)
                   ,(lift-lets-expression alternate))
              pred-binds)))
  (define (lift-lets-from-let bindings body)
    (let per-binding ((bindings bindings)
                      (done null))
      (if (null? bindings)
          (receive (new-body body-binds) (loop body)
            (values new-body (append done body-binds)))
          (let ((binding (car bindings)))
            (receive (new-exp exp-binds) (loop (cadr binding))
              (per-binding
               (cdr bindings)
               (append done
                 (append exp-binds
                   (singleton
                    (car binding) new-exp)))))))))
  (define (lift-lets-from-let-values names sub-exp body)
    ;; TODO Abstract the commonalities with LET forms?
    (receive (new-sub-expr sub-exp-binds) (loop sub-exp)
      (receive (new-body body-binds) (loop body)
        (values new-body
          (append sub-exp-binds
            (append (values-singleton names new-sub-expr)
              body-binds))))))
  (define (lift-lets-from-lambda formals body)
    (values `(lambda ,formals
               ,(lift-lets-expression body))
            null))
  (define (lift-lets-from-application expr)
    ;; In ANF, anything that looks like an application can't have
    ;; nested LETs.
    (values expr null))
  (receive (expr lst) (loop expr) (build expr lst)))

(define (lets-lifted? expr)
  (equal? expr (%lift-lets expr)))
